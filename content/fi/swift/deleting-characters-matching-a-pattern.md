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

## Miksi

Miksi joku haluaisi poistaa tietyntyyppisiä merkkejä koodistaan? Yksi yleinen syy on halu muuttaa tai korjata tiettyä merkkijonoa tai muuttujaa. Esimerkiksi, jos haluat poistaa kaikki välilyönnit tai erikoismerkit tietystä lauseesta tai merkkijonosta, tämä voi olla hyödyllinen tekniikka.

## Miten tehdä

Poistaaksesi merkkejä suoraan Swift-koodeista, voit käyttää `string.replacingOccurrences(of:with:)` -metodia. Tämä metodi korvaa kaikki esiintymät annetulla merkkijonolla tai merkeillä ja palauttaa uuden merkkijonon. Esimerkiksi:

```Swift
let sentence = "Tämä on esimerkkilause!"
let newSentence = sentence.replacingOccurrences(of: "se", with: "pa")

print(newSentence)
// Tulostaa "Tämä on epimerkkilause!"
```

## Syvempi sukellus

Tämä tekniikka ei rajoitu vain yksinkertaisiin korvauksiin, vaan voit myös käyttää säännöllisiä lausekkeita poistamaan merkkejä, jotka täyttävät tietyn kuvion. Voit myös käyttää monimutkaisempia korvaustekniikoita, kuten korvaamista muuttujilla tai funktiokutsuilla.

Lisäksi, jos haluat vain poistaa tietyntyyppisiä merkkejä tietyistä kohdista merkkijonossa, voit käyttää tekstinsisällä uudelleenkirjoittamista (inline string manipulation) käyttäen `string.filter()` -metodia.

## Katso myös

- ["How to replace a string with another string in Swift?" - Stack Overflow](https://stackoverflow.com/questions/27597360/how-to-replace-a-string-with-another-string-in-swift)
- ["Regular Expression in Swift" - Hacking with Swift](https://www.hackingwithswif