---
title:                "Debuggerin käyttö"
date:                  2024-02-03T18:10:21.701556-07:00
model:                 gpt-4-0125-preview
simple_title:         "Debuggerin käyttö"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/go/using-a-debugger.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Mikä ja miksi?

Go-ohjelmoinnissa virheenjäljityksen käyttö tarkoittaa työkalujen tai ominaisuuksien hyödyntämistä ohjelman tilan tarkasteluun ja muokkaamiseen sen käyttäytymisen ymmärtämiseksi tai ongelmien diagnosoimiseksi. Ohjelmoijat tekevät näin tehokkaasti löytääkseen ja korjatakseen bugeja, optimoidakseen suorituskykyä ja varmistaakseen koodinsa oikeellisuuden.

## Kuinka:

Go tarjoaa sisäänrakennetun virheenjäljitystyökalun nimeltä `delve`. Se on täysiverinen virheenjäljitystyökalu, jonka avulla voit suorittaa Go-ohjelmia askel askeleelta, tarkastella ohjelman muuttujia ja arvioida lausekkeita.

Aloittaaksesi sinun täytyy ensin asentaa `delve`. Voit tehdä sen suorittamalla:

```shell
go get -u github.com/go-delve/delve/cmd/dlv
```

Nyt, katsotaanpa kuinka virheenjäljitämme yksinkertaisen Go-ohjelman. Otetaan esimerkiksi ohjelma `main.go`:

```go
package main

import "fmt"

func main() {
    message := "Virheenjäljitys Go:ssa"
    fmt.Println(message)
}
```

Aloittaaksesi tämän ohjelman virheenjäljityksen, avaa terminaali projektikansiossa ja suorita:

```shell
dlv debug
```

Tämä komento kääntää ohjelman optimoinnit pois päältä (parantaakseen virheenjäljityskokemusta), käynnistää sen ja liittää siihen virheenjäljittimen.

Kun `delve` on käynnissä, olet interaktiivisessa virheenjäljittimen kuorissa. Tässä muutama peruskomento:

- `break main.main` asettaa keskeytyskohdan `main`-funktioon.
- `continue` jatkaa ohjelman suoritusta kunnes keskeytyskohta osuu kohdalle.
- `print message` tulostaa `message`-muuttujan arvon.
- `next` vie ohjelman suorituksen seuraavalle riville.
- `quit` poistuu virheenjäljittimestä.

Tuloste, kun keskeytyskohta osuu kohdalle ja muuttujan arvo tulostetaan, saattaa näyttää tältä:

```shell
Breakpoint 1 at 0x49ecf3 for main.main() ./main.go:6
> main.main() ./main.go:6 (hits goroutine(1):1 total:1) (PC: 0x49ecf3)
     1: package main
     2:
     3: import "fmt"
     4:
     5: func main() {
     6: =>    message := "Virheenjäljitys Go:ssa"
     7:       fmt.Println(message)
     8: }
(dlv) print message
"Virheenjäljitys Go:ssa"
```

Näiden komentojen avulla voit astua läpi ohjelmasi, tarkastellen sen tilaa ymmärtääksesi sen käyttäytymistä ja tunnistaaksesi mahdolliset ongelmat.

## Syväsukellus

`Delve`-valinta Go:n virheenjäljitystyökaluksi perinteisten työkalujen, kuten GDB:n (GNU Debugger), sijaan johtuu pääasiassa Go:n suoritusmallista ja ajonaikaisesta ympäristöstä. GDB:tä ei alun perin suunniteltu Go:n ajonaikaisen ympäristön huomioon ottamiseksi, mikä tekee `delvestä` sopivamman valinnan Go-kehittäjille. `Delve` on suunniteltu nimenomaan Go:lle ja tarjoaa intuitiivisemman virheenjäljityskokemuksen Go-rutiineille, kanaville ja muille Go-erityisille rakenteille.

Lisäksi `delve` tukee laajaa ominaisuuksien kirjoa, joita perus-GDB ei tarjoa työskennellessä Go-ohjelmien kanssa. Näihin kuuluu, mutta ei rajoitu: kiinnittyminen käynnissä oleviin prosesseihin virheenjäljitystä varten; ehdolliset keskeytyskohdat; ja monimutkaisten lausekkeiden arviointi, jotka voivat sisältää Go:n rinnakkaisuusprimitiivejä.

Vaikka `delve` on monien Go-kehittäjien valintana virheenjäljittimenä, on syytä huomata, että Go-työkalusarja sisältää myös kevyempiä virheenjäljitystukimuotoja, kuten sisäänrakennetun `pprof`-työkalun profilointiin ja `trace`-työkalun rinnakkaisuuden visualisointiin. Nämä työkalut voivat joskus tarjota nopeamman tai korkeamman tason tavan diagnosoida ohjelman suorituskykyongelmia tai rinnakkaisuusvirheitä, mikä saattaa olla täydentävä tai jopa suositeltavampi riippuen virheenjäljitysyhteydestä.
