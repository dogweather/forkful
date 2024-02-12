---
title:                "Säännöllisten lausekkeiden käyttö"
date:                  2024-02-03T19:18:26.743101-07:00
model:                 gpt-4-0125-preview
simple_title:         "Säännöllisten lausekkeiden käyttö"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/swift/using-regular-expressions.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Mikä & Miksi?
Säännölliset lausekkeet, tai regex, ovat merkkijonoja, jotka muodostavat haun kaavan ja joita käytetään usein merkkijonojen etsimiseen tai manipulointiin. Ohjelmoijat käyttävät niitä kaikkeen tiedon validoinnista ja jäsentämisestä muunnoksiin, tehden niistä korvaamattoman työkalun tekstinkäsittely- ja manipulointitehtävissä eri ohjelmointikielillä, mukaan lukien Swift.

## Miten:
Swiftin natiivi tuki regexille hyödyntää `NSRegularExpression` luokkaa, jonka lisäksi käytetään String-luokan range- ja replacement-metodeja. Alla on esimerkki, kuinka regexiä käytetään löytämään ja korostamaan sähköpostiosoitteet tekstilohkosta:

```swift
import Foundation

let text = "Ota yhteyttä osoitteeseen support@example.com tai feedback@example.org saadaksesi lisätietoja."
let regexPattern = "[A-Z0-9a-z._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}"

try {
    let regex = try NSRegularExpression(pattern: regexPattern)
    let matches = regex.matches(in: text, range: NSRange(text.startIndex..., in: text))

    if !matches.isEmpty {
        for match in matches {
            let range = Range(match.range, in: text)!
            print("Löytyi: \(text[range])")
        }
    } else {
        print("Vastaavuuksia ei löytynyt.")
    }
} catch {
    print("Regex-virhe: \(error.localizedDescription)")
}

// Esimerkkituloste:
// Löytyi: support@example.com
// Löytyi: feedback@example.org
```

Monimutkaisemmissa tai mukavuuteen keskittyvissä skenaarioissa voit käyttää kolmansien osapuolien kirjastoja, kuten SwiftRegex, joka yksinkertaistaa syntaksia ja laajentaa mahdollisuuksia. Vaikka Swiftin vakio kirjasto on voimakas, jotkut kehittäjät suosivat näitä kirjastoja niiden tiiviin syntaksin ja lisäominaisuuksien vuoksi. Tässä on, miten voisit suorittaa samanlaisen tehtävän käyttäen hypoteettista kolmannen osapuolen kirjastoa:

```swift
// Oletetaan, että kirjasto nimeltä SwiftRegex on olemassa ja tuotu käyttöön
let text = "Ota yhteyttä hello@world.com tai vieraile verkkosivustollamme."
let emailPattern = "[A-Z0-9a-z._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}"

let emails = text.matches(for: emailPattern) // Hypoteettinen metodi, jonka SwiftRegex tarjoaa
if emails.isEmpty {
    print("Sähköpostiosoitteita ei löytynyt.")
} else {
    emails.forEach { email in
        print("Löytyi: \(email)")
    }
}

// Hypoteettinen tuloste olettaen, että `matches(for:)` metodi on olemassa SwiftRegex:ssä:
// Löytyi: hello@world.com
```

Tämä esimerkki havainnollistaa kolmannen osapuolen säännöllisen lausekkeen paketin käyttöä yksinkertaistaen vastaavuuksien löytämistä merkkijonosta, olettaen että tällaiset mukavuusmetodit kuten `matches(for:)` ovat olemassa. On tärkeää viitata vastaavan kolmannen osapuolen kirjaston dokumentaatioon tarkan syntaksin ja metodien saatavuuden varmistamiseksi.