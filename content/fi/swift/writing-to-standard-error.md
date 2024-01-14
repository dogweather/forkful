---
title:    "Swift: Kirjoittaminen standardivirheeseen"
keywords: ["Swift"]
---

{{< edit_this_page >}}

## Miksi kirjoittaa standardi virhe?

Useimmat Swift-ohjelmoijat eivät maksele kovin paljon huomiota standardi virheeseen, koska se ei ole yhtä näkyvä kuin vaikkapa konsolin tulostus. Kuitenkin kirjoittaminen standardi virheeseen voi olla hyödyllistä monenlaisissa tilanteissa, kuten virheen käsittelyssä tai debuggaamisessa. Standardi virhe voi auttaa ymmärtämään miksi ohjelma ei toimi odotetusti ja missä kohtaa siinä on virhe. Se voi myös auttaa löytämään koodin mahdollisia haavoittuvuuksia.

## Miten kirjoittaa standardi virhe?

Jos haluat kirjoittaa koodisi standardi virheeseen Swiftillä, sinun tulee käyttää print-funktiota ja ohjata tulostus standardi virheeseen käyttämällä file-parametria. Alla on esimerkki koodista ja sen tekemästä tulosteesta:

```Swift
func divide(number1: Int, number2: Int) throws {
    if number2 == 0 {
        throw DivisionError.divideByZero
    }
    print("Tulos: \(number1/number2)", to: &standardError)
}

enum DivisionError: Error {
    case divideByZero
}

do {
    try divide(number1: 10, number2: 0)
} catch {
    print("Virhe: \(error)", to: &standardError)
}
```

Tuloste: Virhe: divideByZero

Kuten huomaat, olemme ohjanneet tulostuksen standardi virheeseen käyttämällä "&standardError". Tämä tulostaa virheen aina kun se ilmenee.

## Syvempi sukellus

Standardi virhe tukee myös muita muotoiluja, kuten string-muotoilua, jonka avulla voimme tulostaa enemmän tietoa virheestä. Tämä kannattaa erityisesti evaluointivaiheessa, kun yritämme löytää mahdollisia virheitä ohjelmastamme.

Myös standardi virheestä lukeminen on mahdollista tietyin edellytyksin, yleispätevin tapa on käyttää fileprivate set-mallia muuttujalle, jonka tyyppi on:`FileHandle.standardError`. Tämän avulla voimme lukea standardi virheestä, jos tarvitsemme tätä tietoa ohjelmamme suorituksen aikana.

## Katso myös

- Virallinen Swift-dokumentaatio: https://docs.swift.org/swift-book/LanguageGuide/ErrorHandling.html
- Hieno blogipostaus, jossa käydään läpi standardi virheen käyttöä: https://www.swiftbysundell.com/articles/the-power-of-swifts-error-handling/
- Hyödyllisiä vinkkejä ja temppuja standardi virheen käsittelyyn: https://medium.com/flawless-app-stories/a-closer-look-at-throwing-functions-in-swift-10a36243a5c1