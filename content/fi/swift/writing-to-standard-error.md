---
title:                "Kirjoittaminen standardilähtöön"
html_title:           "Swift: Kirjoittaminen standardilähtöön"
simple_title:         "Kirjoittaminen standardilähtöön"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/swift/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Miksi

Kirjoittaminen standard erroriin voi olla hyödyllistä silloin, kun halutaan tulostaa virheilmoituksia tai lokimerkintöjä, jotka eivät haluta näkyvän käyttäjän näytöllä.

## Miten

Kirjoittaminen standard erroriin onnistuu käyttämällä Swiftin *print()* -funktiota ja siihen liittyvää parametria *standardError*. Esimerkkejä ja tulostuksia alla:

```Swift
print("Tämä tulostuu standard outputiin.")
print("Tämä tulostuu standard erroriin.", to: &standardError)
```

Tulostus:

```
Tämä tulostuu standard outputiin.
Tämä tulostuu standard erroriin.
```

Voimme myös luoda uuden *FileHandle* -objektin ja liittää sen *standardError* -muuttujaan, jolloin voimme kirjoittaa suoraan haluttuun tiedostoon:

```Swift
let file = FileHandle(forWritingAtPath: "/polku/tiedostoon")
standardError = file
print("Tämä tulostuu tiedostoon.", to: &standardError)
```

## Syväsukellus

Swiftin standardikirjaston *print()* -funktio käyttää taustalla *FileOutputStream* -tyyppistä streamiä tietojen kirjoittamiseen *standardError* -objektiin. Tällä tavoin voimme helposti ohjata virheilmoituksia tai lokimerkintöjä muualle, esimerkiksi tiedostoon. On myös mahdollista luoda omia *OutputStream* -objekteja ja liittää ne *standardError* -muuttujaan.

## Katso myös

- [Swiftin virallinen dokumentaatio](https://docs.swift.org/swift-book/LanguageGuide/BasicOperators.html#ID551)
- [FileHandle -luokan dokumentaatio](https://developer.apple.com/documentation/foundation/filehandle)
- [FileOutputStream -luokan dokumentaatio](https://developer.apple.com/documentation/foundation/fileoutputstream)