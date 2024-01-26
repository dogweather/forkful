---
title:                "Virheiden käsittely"
date:                  2024-01-26T00:58:06.022705-07:00
model:                 gpt-4-1106-preview
simple_title:         "Virheiden käsittely"
programming_language: "Swift"
category:             "Swift"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/swift/handling-errors.md"
---

{{< edit_this_page >}}

## Mikä ja Miksi?
Virheenkäsittely Swiftissä tarkoittaa ongelmien ennakointia ja ratkaisemista, jotka ilmaantuvat koodisi ajon aikana. Teemme niin kaaoksen hallitsemiseksi—pitäen sovellukset kaatumattomina ja tarjoten käyttäjälle sujuvan kokemuksen.

## Kuinka toimia:
Swift käyttää virheenkäsittelyyn `do`, `try` ja `catch` -lohkoja. Katsotaanpa esimerkkiä:

```Swift
enum FileError: Error {
    case fileDoesNotExist
    case noPermission
}

func readFile(atPath path: String) throws -> String {
    // Kuvittele, että meillä on tässä jonkinlaista logiikkaa tiedoston olemassaolon ja lukuoikeuden tarkistamiseen
    let fileExists = false
    let havePermission = true

    if !fileExists {
        throw FileError.fileDoesNotExist
    }

    if !havePermission {
        throw FileError.noPermission
    }

    return "Tiedoston sisältö tulee tähän"
}

do {
    let fileContent = try readFile(atPath: "/polku/tiedostoon")
    print(fileContent)
} catch FileError.fileDoesNotExist {
    print("Hupsista! Tiedostoa ei löydy.")
} catch FileError.noPermission {
    print("Ah! Ei lukuoikeutta tiedostoon.")
} catch {
    print("Tuntematon virhe tapahtui.")
}

```

Esimerkkituloste:

```
Hupsista! Tiedostoa ei löydy.
```

## Syväsukellus
Virheenkäsittely ei aina ollut yhtä sujuvaa kuin nyt. Objective-C:ssä käsiteltiin NSError-objektien osoittimia, mikä tuntui kömpelöltä. Nyt meillä on tyylikkäämpi järjestelmä Swiftin enumien ja `Error`-protokollan ansiosta.

Swiftin `throw` antaa meille mahdollisuuden viestiä, että jokin meni pieleen. `do`-lohkot toimivat kuin virheentietoisina alueina, `try`-etuliite kutsuu riskialttiita toimintoja, ja `catch` käsittelee asiat, jos ne menevät mönkään.

Optionaalit ovat vaihtoehto tilanteille, jotka eivät aivan yllä "virhe"-statukseen, mutta saattavat silti olla "tulos puuttuu". Ne ovat vähän kuin Schrödingerin muuttujat—niillä on arvo, tai sitten ei.

Todelliseen syvyyteen tutustu `Result`-tyyppeihin, jotka ovat hienostuneita hybridejä tavallisten paluu- ja virhemallien välillä.

## Katso myös
- Virallinen Swiftin virheenkäsittelyopas: [Apple Docs](https://docs.swift.org/swift-book/LanguageGuide/ErrorHandling.html)
- Parhaat käytännöt Swiftin virheenkäsittelyssä: [RayWenderlich.com](https://www.raywenderlich.com/1851-beginning-swift-error-handling)
- Edistynyt virheenkäsittely Swiftissä: [Medium Artikkeli](https://medium.com/better-programming/advanced-error-handling-in-swift-4f6bdf6b01d8)