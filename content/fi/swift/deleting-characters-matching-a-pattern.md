---
title:                "Mallia vastaavien merkkien poistaminen"
html_title:           "Swift: Mallia vastaavien merkkien poistaminen"
simple_title:         "Mallia vastaavien merkkien poistaminen"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/swift/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?
Miksi poistamme merkkejä, jotka vastaavat tiettyä kuvioa? Tätä tehdään ohjelmoinnissa, kun haluamme puhdistaa tai muokata tiettyä tekstiä tai dataa. Tämä on hyödyllistä esimerkiksi silloin, kun haluamme poistaa ylimääräisiä välilyöntejä tai muokata syötettä haluttuun muotoon.

## Kuinka:
Tässä esimerkissä käytämme String-tyypin `replacingOccurrences`-metodia poistaaksemme kaikki numerot merkkijonosta `text`.

```Swift
let text = "H3ll0 W0rld!"
let noNumbers = text.replacingOccurrences(of: "[0-9]", with: "", options: .regularExpression)

print(noNumbers) // Tulostaa “Hll Wrld!”
```

## Syväsukellus:
Historiallisesta kontekstista, poistamalla merkkejä halutun kuvion perusteella on ollut pitkään käytetty ohjelmointiin. Ennen regex-järjestelmän esittelyä ohjelmoijien täytyi käyttää monimutkaisia kaavoja ja tietorakenteita tämän saavuttamiseksi. Nykyään käytämme yleisesti säännöllisiä lausekkeita eli regexejä, jotka tarjoavat helppokäyttöisen ja tehokkaan tavan poistaa merkkejä kuvioittain. Tämän lisäksi on olemassa myös muita vaihtoehtoisia tapoja poistaa merkkejä, kuten käyttämällä looppeja ja merkkijonojen metodien yhdistelmää.

## Katso myös:
- [NSString Regular Expressions - Apple Developer Documentation](https://developer.apple.com/documentation/foundation/nsstring/1411947-replacingcharacters)
- [Swift Regular Expressions - Stack Overflow](https://stackoverflow.com/questions/27880650/swift-extract-regex-matches#27880421)
- [Learn Regular Expressions Basics - RegexOne](https://regexone.com/)