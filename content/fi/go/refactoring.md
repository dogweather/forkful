---
title:                "Koodin uudelleenjärjestely"
date:                  2024-01-26T01:35:05.416370-07:00
model:                 gpt-4-0125-preview
simple_title:         "Koodin uudelleenjärjestely"

category:             "Go"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/go/refactoring.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?
Refaktorointi on prosessi, jossa olemassa olevaa tietokonekoodia uudelleenjärjestetään muuttamatta sen ulkoista käyttäytymistä. Ohjelmoijat tekevät sen parantaakseen ohjelmiston ei-toiminnallisia attribuutteja, kuten luettavuutta ja ylläpidettävyyttä, mikä voi tehdä koodista helpommin ymmärrettävää, vähentää monimutkaisuutta ja auttaa löytämään bugeja helpommin.

## Kuinka:
Sukelletaan yksinkertaiseen Go-koodin refaktorointiesimerkkiin. Otamme katkelman, joka laskee lukujonon keskiarvon, ja refaktoroimme sen selkeyden ja uudelleenkäytettävyyden vuoksi.

Alkuperäinen koodi:
```Go
package main

import "fmt"

func main() {
    numbers := []float64{8, 12, 15, 10, 7, 14}
    var sum float64
    for _, num := range numbers {
        sum += num
    }
    average := sum / float64(len(numbers))
    fmt.Println("Average:", average)
}
```

Refaktoroitu koodi:
```Go
package main

import "fmt"

// CalculateAverage ottaa vastaan float64-taulukon ja palauttaa keskiarvon.
func CalculateAverage(numbers []float64) float64 {
    sum := 0.0
    for _, num := range numbers {
        sum += num
    }
    return sum / float64(len(numbers))
}

func main() {
    numbers := []float64{8, 12, 15, 10, 7, 14}
    average := CalculateAverage(numbers)
    fmt.Println("Average:", average)
}
```

Refaktoroidussa koodissa olemme erottaneet keskiarvon laskemisen logiikan erilliseen funktioon nimeltä `CalculateAverage`. Tämä tekee `main`-funktiosta ytimekkäämmän ja keskiarvon laskennan logiikan uudelleenkäytettävän ja testattavan.

## Syväsukellus
Koodin refaktorointi ei ole moderni konsepti; se on ollut olemassa ennen laajalle levinnyttä tietokoneiden käyttöä. Käytäntö on luultavasti alkanut mekaanisen insinöörityön alalla tai vielä aiemmin. Ohjelmistossa siitä tuli muodollisempi objekti-ohjelmoinnin ja extreme-ohjelmoinnin (XP) myötä 1990-luvulla, merkittävästi vaikutteita saaden Martin Fowlerin perustavanlaatuisesta kirjasta "Refactoring: Improving the Design of Existing Code."

Refaktorointitekniikoita on lukuisia, yksinkertaisesta muuttujien uudelleennimeämisestä selkeyden vuoksi monimutkaisempiin kaavoihin kuten metodien tai luokkien erottamiseen. Avain on tehdä pieniä, inkrementaalisia muutoksia, jotka eivät muuta ohjelmiston toiminnallisuutta mutta parantavat sisäistä rakennetta.

Go:n käyttäessä refaktorointi voi olla suoraviivaista kielen yksinkertaisuuden ja tehokkaan standardikirjaston ansiosta. Kuitenkin hyvän yksikkötestien joukon omistaminen on edelleen tärkeää varmistamaan, etteivät refaktoroinnit tuo mukanaan bugeja. Työkalut kuten `gorename` ja `gofmt` auttavat automatisoimaan osan prosessista, ja IDE:illä on usein sisäänrakennettu refaktorointituki.

Manuaalisen refaktoroinnin lisäksi Go:lle on saatavilla muutamia automatisoituja koodin refaktorointityökaluja, kuten GoLandin refaktorointityökalut ja Go Refactor. Vaikka nämä voivat nopeuttaa prosessia, ne eivät ole korvike koodin ymmärtämiselle ja harkittujen muutosten tekemiselle.

## Katso myös
 - [Refaktorointi Go:lla: Yksinkertainen on kaunista](https://go.dev/blog/slices)
 - [Tehokas Go: Refaktorointi rajapintojen avulla](https://go.dev/doc/effective_go#interfaces)
 - [Martin Fowlerin Refaktorointi-sivu](https://refactoring.com/)
 - [GoLand Refaktorointityökalut](https://www.jetbrains.com/go/features/refactorings/)
