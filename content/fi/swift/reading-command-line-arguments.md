---
title:                "Swift: Komentoriviparametrien lukeminen"
simple_title:         "Komentoriviparametrien lukeminen"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/swift/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Miksi

Swift-ohjelmointi tarjoaa monia hyödyllisiä työkaluja, joilla voi helpottaa ohjelmien suorittamista eri ympäristöissä. Yksi näistä työkaluista on komentoriviparametrien lukeminen, joka mahdollistaa käyttäjän antamien arvojen hyödyntämisen ohjelman suorituksessa. Tässä blogikirjoituksessa käymme läpi, miten voit helposti lukea komentoriviparametreja Swift-ohjelmassa.

## Näin

 ```Swift
import Foundation

let arguments = CommandLine.arguments
// arguments sisältää kaikki komentoriviparametrit taulukkona

// Tulostetaan parametrit yksi kerrallaan
for argument in arguments {
    print(argument)
}

// Jos haluat tietyn parametrin arvon, voit käyttää seuraavaa koodilohkoa:
if arguments.count > 1 {
    let value = arguments[1]
    print("Arvo: \(value)")
}

 ```

Kun suoritat tämän koodin ja annat komentoriville esimerkiksi seuraavanlaisia parametreja: ```Swift ./ohjelma Argumentti1 Argumentti 2```, tulostetaan konsoliin seuraavaa:

```
./ohjelma
Argumentti1
Argumentti2
```

## Syväsukellus

Komentoriviparametrien lukeminen on toteutettu Swiftissä CommandLine-objektilla, joka tarjoaa tilapäistaulukon CommandLine.arguments kaikilla parametreilla. Voit myös käyttää CommandLine.argc ja CommandLine.unsafeArgv -muuttujia, jotka tarjoavat vastaavat tiedot kuin argc ja argv C-kielellä. Voit lukea tarkemmin CommandLine-objektista Swiftin dokumentaatiosta.

## Katso myös

- [Apple Developer Documentation: CommandLine](https://developer.apple.com/documentation/foundation/commandline)
- [Myös vinkkejä Swift-ohjelmoinnista löydät täältä](https://smartik.es/swift/)

Kiitos lukemisesta ja onnea komentoriviparametrien lukemiseen Swift-ohjelmillasi! 🚀