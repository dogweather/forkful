---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:59:52.642342-07:00
description: "Kuinka: Go:ssa funktio m\xE4\xE4ritell\xE4\xE4n k\xE4ytt\xE4m\xE4ll\xE4\
  \ `func`-avainsanaa, jonka j\xE4lkeen tulee funktion nimi, parametrit (jos niit\xE4\
  \ on) ja palautustyyppi.\u2026"
lastmod: '2024-03-13T22:44:56.056579-06:00'
model: gpt-4-0125-preview
summary: "Go:ssa funktio m\xE4\xE4ritell\xE4\xE4n k\xE4ytt\xE4m\xE4ll\xE4 `func`-avainsanaa,\
  \ jonka j\xE4lkeen tulee funktion nimi, parametrit (jos niit\xE4 on) ja palautustyyppi."
title: "Koodin j\xE4rjest\xE4minen funktioihin"
weight: 18
---

## Kuinka:
Go:ssa funktio määritellään käyttämällä `func`-avainsanaa, jonka jälkeen tulee funktion nimi, parametrit (jos niitä on) ja palautustyyppi. Esitellään asia yksinkertaisella esimerkillä:

```go
package main

import "fmt"

// määrittele funktio kahden luvun summan laskemiseksi
func addNumbers(a int, b int) int {
    return a + b
}

func main() {
    sum := addNumbers(5, 7)
    fmt.Println("Summa on:", sum)
    // Tuloste: Summa on: 12
}
```

Funktiot voivat myös palauttaa useita arvoja, mikä on ainutlaatuinen ominaisuus verrattuna moniin muihin kieliin. Tässä on esimerkki siitä, miten voit hyödyntää tätä:

```go
// määrittele funktio kahden luvun vaihtamiseksi
func swap(a, b int) (int, int) {
    return b, a
}

func main() {
    x, y := swap(10, 20)
    fmt.Println("x, y vaihdon jälkeen:", x, y)
    // Tuloste: x, y vaihdon jälkeen: 20 10
}
```

Voit myös määritellä funktioita, joilla on vaihteleva määrä argumentteja käyttämällä ellipsiä `...` ennen parametrityyppiä. Tämä on hyödyllistä joustavien funktioiden luomisessa:

```go
// määrittele funktio tuntemattoman määrän kokonaislukujen summan laskemiseksi
func sum(numbers ...int) int {
    total := 0
    for _, number := range numbers {
        total += number
    }
    return total
}

func main() {
    total := sum(1, 2, 3, 4, 5)
    fmt.Println("Kokonaissumma on:", total)
    // Tuloste: Kokonaissumma on: 15
}
```

## Syvä sukellus
Koodin järjestäminen funktioihin ei ole omituista vain Go:lle—se on perusohjelmointiperiaate. Go kuitenkin tuo tiettyjä käytäntöjä ja kykyjä, jotka erottavat sen funktionhallinnassa. Esimerkiksi kyky palauttaa useita arvoja funktioista on suhteellisen ainutlaatuinen ja voi johtaa puhtaampaan, ymmärrettävämpään koodiin, erityisesti kun käsitellään operaatioita, jotka perinteisesti saattaisivat vaatia osoittimien käyttöä tai poikkeusten käsittelyä.

Lisäksi Go:n tuki ensiluokan funktioille—funktioille, joita voidaan välittää argumentteina muihin funktioihin, palauttaa arvoina funktioista ja sijoittaa muuttujiin—parantaa kielen tukea funktionaalisille ohjelmointimalleille. Tämä ominaisuus on erityisen hyödyllinen korkeamman tason funktioiden luomisessa, jotka manipuloivat tai yhdistävät muita funktioita.

On kuitenkin tärkeää olla tietoinen "vähenevän tuoton laista", kun järjestetään koodia funktioihin. Liiallinen modulaarisuus voi johtaa liialliseen abstraktioon, mikä tekee koodista vaikeamman ymmärtää ja ylläpitää. Lisäksi, vaikka Go:n yksinkertainen lähestymistapa virheenkäsittelyyn (virheiden palauttaminen normaaleina paluuarvoina) rohkaisee selkeään virheiden leviämiseen useiden funktion kutsutasojen läpi, se voi johtaa toistuvaan virheidenkäsittelykoodiin. Vaihtoehdot, kuten virheenkäsittelykehykset tai muiden kielten "try-catch" -lähestymistavan omaksuminen (vaikka se ei olekaan natiivisti tuettuna) pakettitoteutusten kautta, voivat joskus tarjota elegantimpia ratkaisuja riippuen käyttötapauksesta.

Päätös funktioiden ja modulaarisuuden laajasta hyödyntämisestä Go:ssa tulisi tasapainottaa abstraktion, ylläpidettävyyden, suorituskyvyn ja luettavien virheenkäsittelyjen tarpeen välillä, hyödyntäen parhaiten Go:n suoraviivaisten, mutta tehokkaiden ominaisuuksien tarjoamia etuja.
