---
title:    "Swift: Tekstin etsiminen ja korvaaminen"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/swift/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Miksi
Miksi koodaajat usein turvautuvat tekstin etsimiseen ja korvaamiseen? Koska se on nopea ja tehokas tapa muuttaa useita tiedostoja kerralla. Se on myös kätevä tapa korjata virheitä tai päivittää vanhentuneita koodinpätkiä. Joten, olitpa aloittelija tai kokenut koodaaja, on tärkeää hallita tekstin etsiminen ja korvaaminen tekniikka.

## Kuinka tehdä
Etsimisen ja korvaamisen tekniikan hallitseminen Swiftissä on helppoa ja tehokasta. Ensinnäkin, tarvitset tekstin, jota haluat etsiä ja korvata. Voit käyttää `replacingOccurrences` -funktiota etsimään kaikki esiintymät ja korvaamaan ne haluamallasi tekstillä. Alla on esimerkki, jossa etsitään ja korvataan tekstin "he" muuttujassa `quote` olevalla tekstillä "she".

```Swift
let quote = "He loves to code in Swift."
let replacedQuote = quote.replacingOccurrences(of: "he", with: "she")
print(replacedQuote)
```
Tulostaa: "She loves to code in Swift."

Voit myös käyttää `replacingCharacters` -funktiota, jotta voit tarkasti määrittää, mitkä merkit haluat korvata. Alla olevassa esimerkissä korvataan kaikki numerot merkillä "-".

```Swift
let numbers = "1234567"
let replacedNumbers = numbers.replacingCharacters(in: numbers.startIndex...numbers.endIndex, with: "-")
print(replacedNumbers)
```
Tulostaa: "-------"

## Syvällinen sukellus
Swiftin tekstinkäsittelystä löytyy paljon muitakin toiminnallisuuksia, kuten `replacingOccurrences` -funktion erilaisia muunnelmia ja `String`-tyypin lisätoimintoja, jotka voivat tulla tarpeellisiksi tekstien muokkaamisessa. On myös tärkeää ymmärtää, että tekstiä voi etsiä ja korvata sekä `String`- että `NSMutableString`-tyypeissä. On hyvä tutustua dokumentaatioon, jotta voit löytää sinulle sopivimmat työkalut ja käytännöt.

## Katso myös
- [Replacing characters in a string](https://developer.apple.com/documentation/foundation/nsstring/1413732-replacingcharacters)
- [Replacing occurrences in a string](https://developer.apple.com/documentation/foundation/nsstring/1418337-replacingoccurrences)