---
title:                "Leser en tekstfil"
html_title:           "Swift: Leser en tekstfil"
simple_title:         "Leser en tekstfil"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/swift/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å lese en tekstfil innebærer å åpne og få tilgang til innholdet i en fil som er lagret som ren tekst. Dette er en vanlig oppgave for programmører når de jobber med datafiler eller konfigurasjonsfiler som trengs for å kjøre et program.

## Hvordan:
```Swift
if let fileURL = Bundle.main.url(forResource: "tekstfil", withExtension: "txt") {
    do {
        let tekst = try String(contentsOf: fileURL, encoding: .utf8)
        print(tekst)
    } catch {
        print("Kunne ikke lese filen")
    }
}
```
Output:
```
Dette er eksempeltekst som er lagret i en tekstfil.
Programmet mitt kan nå få tilgang til denne teksten og behandle den som jeg ønsker.
```

## Dykk Dypere:
Det å lese tekstfiler har vært en grunnleggende operasjon i programmering i mange år. Før i tiden, når datamaskiner var mye mindre og mindre avanserte, var det vanlig å lagre data i tekstfiler. Alternativer til å lese en tekstfil inkluderer å bruke forskjellige datatyper og formater, som XML eller JSON. Implementeringen av å lese en tekstfil vil variere avhengig av programmeringsspråket og plattformen du bruker.

## Se også:
- [How to Work with Text Files in Swift](https://www.raywenderlich.com/8114966-how-to-work-with-text-files-in-swift)
- [Reading and Writing Text Files in Swift](https://www.swiftbysundell.com/articles/reading-and-writing-files-in-swift/)
- [The Basics of Handling Files in Swift](https://www.hackingwithswift.com/read/0/5/the-basics-of-handling-files-in-swift)