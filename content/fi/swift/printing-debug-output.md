---
title:                "Swift: Tulostaminen vianetsintä tulos"
programming_language: "Swift"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/swift/printing-debug-output.md"
---

{{< edit_this_page >}}

## Miksi

Miksi tulostuksia vianmääritykseen on tärkeä osa Swift-ohjelmointia?

On monia syitä, miksi tulostuksia vianmääritykseen käytetään ohjelmoijien keskuudessa. Se voi auttaa selvittämään virheitä, jäljittämään ohjelman suoritusjärjestystä ja varmistamaan, että koodissa ei ole käyttäjälle näkymättömiä ongelmia. Lisäksi se voi auttaa ymmärtämään paremmin ohjelman toimintaa ja suorituskykyä.

## Miten

Tässä esimerkissä näytämme, miten voit käyttää tulostuksia vianmääritykseen Swift-ohjelmassa.

```
Swift.print("Tämä on tulostus vianmääritykseen.")
```

Tulostus voi sisältää myös muuttujien arvoja, jotta voit seurata niiden arvojen muutoksia ohjelman suorituksen aikana.

```
var nimi = "Matti"
var ikä = 35
Swift.print("Hei, olen \(nimi) ja olen \(ikä) vuotta vanha.")
```

Esimerkkituloste:

```
Hei, olen Matti ja olen 35 vuotta vanha.
```

Voit myös käyttää määriteltyjä funktioita ja niiden tulostuksia vianmääritykseen.

```
func lasku(_ x: Int, _ y: Int) -> Int {
    return x + y
}

Swift.print(lasku(5, 3))
```

Esimerkkituloste:

```
8
```

## Syvemmälle

Tulostuksilla vianmääritykseen on mahdollista tehdä paljon muutakin kuin vain tulostaa tietoa konsoliin. Voit esimerkiksi kirjoittaa tulostuksia tiedostoon tai siirtää ne muihin ohjelman osiin, kuten virheenkäsittelyyn. Lisäksi voit käyttää erilaisia tulostusmetodeja, kuten `debugPrint` ja `dump`, jotka tarjoavat lisää tietoa tyypinmäärityksestä ja rakenteesta.

Tulostuksia vianmääritykseen voi myös käyttää ohjelmien optimointiin ja suorituskyvyn parantamiseen. Voit esimerkiksi testata erilaisia koodikingasrakenteita ja verrata niiden suoritusaikoja tulostuksien avulla.

## Katso myös

- [Swiftin virallinen dokumentaatio](https://docs.swift.org/swift-book/LanguageGuide/TheBasics.html)
- [Swiftin debugPrint-metodi ja sen käyttö](https://developer.apple.com/documentation/swift/1541053-debugprint)
- [Swiftin dump-metodi ja sen käyttö](https://developer.apple.com/documentation/swift/1541123-dump)
- [Ohjelmien optimointi Swiftissä](https://www.raywenderlich.com/1415826-optimizing-swift-performance)