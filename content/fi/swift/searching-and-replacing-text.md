---
title:                "Swift: Etsiminen ja tekstin vaihtaminen"
programming_language: "Swift"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/swift/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Miksi

On monia tilanteita, joissa joudut muokkaamaan tekstiä ohjelmointiprojektissasi. Kenties sinun täytyy korvata tietyt sanat tai lauseet tietyillä tiedoilla, tai ehkä haluat yksinkertaisesti vaihtaa tietyn tekstin kokonaan toisella tekstillä. Onneksi Swiftillä on tehokas työkalu, joka auttaa meitä suorittamaan nämä tehtävät: hakeminen ja korvaaminen.

## Kuinka tehdä

Hakeminen ja korvaaminen ovat yksinkertaisia ja helppoja tehtäviä Swiftissä. Voit käyttää `replacingOccurrences(of:with:)` -funktiota muuttaaksesi tiettyjä sanoja tai lausekkeita haluamallasi tavalla. Katso alla olevaa koodiesimerkkiä:

```Swift
let teksti = "Tässä on esimerkki tekstistä, jossa esiintyy sana kaupunki."

let uusiTeksti = teksti.replacingOccurrences(of: "kaupunki", with: "maaseutu")

print(uusiTeksti)

// Tulostaa: "Tässä on esimerkki tekstistä, jossa esiintyy sana maaseutu."
```

Kuten näet, `replacingOccurrences(of:with:)` -funktio korvaa kaikki esiintymät haluamallasi tavalla. Voit myös käyttää `replacingOccurrences(of:with:options:)` -funktiota muuttamaan hakutoimintojen tarkkuutta. Voit esimerkiksi lisätä `.caseInsensitive` -asetuksen, jotta hakemiseen tulee mukaan myös isot ja pienet kirjaimet.

## Syvemmälle

Swift tarjoaa myös muita vaihtoehtoja tekstien hakemiseen ja korvaamiseen, kuten `range(of:)` ja `replaceSubrange(_:)`. Voit myös käyttää säännöllisiä lausekkeita tekstikäsittelyyn.

Hakeminen ja korvaaminen ovat tärkeitä taitoja ohjelmoinnissa, ja Swiftillä niiden toteuttaminen on helppoa ja tehokasta.

## Katso myös

- [Swiftin virallinen dokumentaatio hakemisesta ja korvaamisesta](https://developer.apple.com/documentation/foundation/nsstring/1417744-replacingoccurrences)
- [Hakeminen ja korvaaminen säännöllisillä lausekkeilla Swiftissä](https://www.hackingwithswift.com/articles/108/regular-expressions-in-swift-tutorial)