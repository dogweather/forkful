---
title:                "Debug-tulosteen tulostaminen"
html_title:           "Bash: Debug-tulosteen tulostaminen"
simple_title:         "Debug-tulosteen tulostaminen"
programming_language: "Swift"
category:             "Swift"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/swift/printing-debug-output.md"
---

{{< edit_this_page >}}

# Debug-tulostuksen merkitys ja tehtävä Swiftissä

## Mitä & Miksi?

Debug-tulostus on tietoa, jonka ohjelmanne välittää ymmärtääkseen, miten koodin eri osat suorittavat. Se auttaa nopeuttamaan ongelmanratkaisuprosessia ja tehostaa ohjelman suorituskykyä.

## Kuinka:

Swiftissä `print`-toimintoa käytetään yleisesti tulostamaan debug-tietoja. Alkuperäinen viesti voidaan tulostaa sekä lokiin että konsoliin.

```Swift
let name = "Jere"
print("Hello, my name is \(name)")
```

Esim. koodin tuloste näyttää tältä:

```Swift
Hello, my name is Jere
```

## Syvempi tutkinta:

Historiassa, debug-tulostus alkoi COBOL- ja FORTRAN-kieleillä, jotka sisälsivät löytyjen virheiden tutkintaan tarkoitetut toiminnot. Swiftissä meillä on vielä enemmän hallintaa tulosteen muodossa näkyvien tietojen mukaan.

Vaihtoehtoja debug-tulostukselle ovat esimerkiksi `debugPrint()` ja `dump()`. Ne näyttävät ylimääräisiä yksityiskohtia alustettavissa olevista esineistä.

```Swift
var array = ["Apple", "Banana", "Cherry"]
debugPrint(array)

dump(array)
```

Ensimmäinen `debugPrint()` tulostaa tutun Swift-kokoelman. Toinen `dump()` paljastaa vieläkin enemmän yksityiskohtia.

## Katso myös:

Lisätietoja debug-tulostuksesta Swiftissä voi löytää seuraavilta sivustoilta:

* [Apple Developer Documentation](https://developer.apple.com/documentation/swift/1541053-print)
* [Swift by Sundell](https://www.swiftbysundell.com)
* [Stack Overflow - Swift](https://stackoverflow.com/questions/tagged/swift)

Näec opettelemaan ja koodaamaan!