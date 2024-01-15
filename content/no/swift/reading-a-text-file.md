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

## Hvorfor

Det å lese en tekstfil er en viktig ferdighet innen programmering, spesielt når man jobber med å håndtere store mengder data eller behandle informasjon fra eksterne kilder. Ved å lese en tekstfil kan man hente ut informasjon og behandle den i programmene sine, noe som gjør koden mer fleksibel og dynamisk.

## Slik gjør du det

For å lese en tekstfil i Swift, kan du bruke funksjonen `contentsOfFile` fra `NSString`-klassen. Her er et eksempel på hvordan dette kan gjøres:

```Swift
if let path = Bundle.main.path(forResource: "tekstfil", ofType: "txt") {
    do {
        let data = try String(contentsOfFile: path, encoding: .utf8)
        print(data)
    } catch {
        print(error)
    }
}
```

I dette eksempelet har vi først brukt funksjonen `path(forResource:ofType:)` for å få tak i filbanen til tekstfilen vår. Deretter har vi brukt `String(contentsOfFile:encoding:)` for å lese informasjonen fra filen og lagre den i en variabel. Til slutt skriver vi ut denne informasjonen til konsollen.

## Dykk dypere

Det finnes også andre måter å lese tekstfiler på i Swift, som for eksempel å bruke en "stream reader" eller å bruke tredjepartsbiblioteker. Det er viktig å huske på at når man leser en tekstfil, må man ha riktig filbane og at man må håndtere eventuelle feil som kan oppstå underveis.

## Se også

- [Les og skriv til tekstfiler i Swift](https://www.hackingwithswift.com/articles/77/how-to-read-and-write-to-the-file-system-using-nsfilemanager)
- [Apples offisielle dokumentasjon om å lese og skrive til filer](https://developer.apple.com/documentation/foundation/file_manager)
- [Tredjepartsbiblioteker for å håndtere tekstfiler i Swift](https://stackify.com/swift-libraries/)