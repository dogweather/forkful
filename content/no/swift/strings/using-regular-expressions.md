---
title:                "Bruke regulære uttrykk"
date:                  2024-02-03T19:18:52.291826-07:00
model:                 gpt-4-0125-preview
simple_title:         "Bruke regulære uttrykk"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/swift/using-regular-expressions.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Regulære uttrykk, eller regex, er sekvenser av tegn som danner et søkemønster, ofte brukt til oppgaver som sammenligning eller manipulasjon av tekststrenger. Programmerere benytter dem til alt fra datavalidering og parsing til transformasjoner, noe som gjør dem til et uunnværlig verktøy i tekstbehandlings- og manipuleringsoppgaver på tvers av ulike programmeringsspråk, inkludert Swift.

## Hvordan:
Swifts innebygde støtte for regex benytter klassen `NSRegularExpression`, sammen med String-klassens område- og erstatningsmetoder. Nedenfor er et eksempel på bruk av regex for å finne og markere e-postadresser i en tekstblokk:

```swift
import Foundation

let text = "Kontakt oss på support@example.com eller feedback@example.org for mer informasjon."
let regexMønster = "[A-Z0-9a-z._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}"

do {
    let regex = try NSRegularExpression(pattern: regexMønster)
    let treff = regex.matches(in: text, range: NSRange(text.startIndex..., in: text))

    if !treff.isEmpty {
        for match in treff {
            let range = Range(match.range, in: text)!
            print("Fant: \(text[range])")
        }
    } else {
        print("Ingen treff funnet.")
    }
} catch {
    print("Regex-feil: \(error.localizedDescription)")
}

// Eksempel på utdata:
// Fant: support@example.com
// Fant: feedback@example.org
```

For mer komplekse eller bekvemmelighetsfokuserte scenarioer, kan du bruke tredjepartsbiblioteker som SwiftRegex, som forenkler syntaksen og utvider mulighetene. Selv om Swifts standardbibliotek er kraftig, foretrekker noen utviklere disse bibliotekene for deres konsise syntaks og ekstra funksjoner. Slik kan du utføre en lignende oppgave ved hjelp av et hypotetisk tredjepartsbibliotek:

```swift
// Under antagelsen om at et bibliotek kalt SwiftRegex eksisterer og er importert
let text = "Ta kontakt på hello@world.com eller besøk nettstedet vårt."
let epostMønster = "[A-Z0-9a-z._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}"

let eposter = text.matches(for: epostMønster) // Hypotetisk metode levert av SwiftRegex
if eposter.isEmpty {
    print("Ingen e-postadresser funnet.")
} else {
    eposter.forEach { epost in
        print("Fant: \(epost)")
    }
}

// Hypotetisk utdata antar at `matches(for:)`-metoden eksisterer i SwiftRegex:
// Fant: hello@world.com
```

Dette eksemplet illustrerer bruken av et tredjeparts regulært uttrykkspakke for å forenkle finningen av treff i en streng, under antagelsen om at slike bekvemmelighetsmetoder som `matches(for:)` eksisterer. Det er viktig å henvise til den respektive tredjepartsbiblioteksdokumentasjonen for nøyaktig syntaks og metode tilgjengelighet.