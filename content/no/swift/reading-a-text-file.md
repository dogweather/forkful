---
title:                "Lese en tekstfil"
html_title:           "C#: Lese en tekstfil"
simple_title:         "Lese en tekstfil"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/swift/reading-a-text-file.md"
---

{{< edit_this_page >}}

# Lesing av tekstfil i Swift

## Hva & Hvorfor?
Å lese en tekstfil er å hente dataene som er lagret i filen for behandling. Programmerere gjør dette for å manipulere eller analysere data, drive med maskinlæring, eller til og med for å hente inn konfigurasjoner.

## Hvordan:
For å lese en fil i Swift, kan vi bruke `String` klassen med metoden `init(contentsOfFile:)`. Her er et eksempel:

```Swift
let fileUrl = Bundle.main.url(forResource: "YourFileName", withExtension: "txt")
do {
    let text = try String(contentsOf: fileUrl!, encoding: .utf8)
    print(text)
} catch {
    print("Error: cannot read the file")
}
```
Når vi kjører dette scriptet og filen inneholder "Swift er fantastisk", vil outputen være:

```Swift
Swift er fantastisk
```

## Dypdykk
Historisk har det vært flere måter å lese filer på i forskjellige programmeringsspråk, hvorav hver har sine fordeler og ulemper. Men i Swift, er det primært to metoder. Den vi diskuterte tidligere bruker `String`-klassen. Alternativt kan vi også bruke `FileHandle`-klassen.

`FileHandle` gir en dypere kontroll over hvordan vi vil håndtere en fil, men koden blir mer kompleks. På den annen side, er `String(contentsOfFile:)`-metoden mer kompakt, men gir mindre fleksibilitet.

## Se Også
For mer informasjon om filhåndtering i Swift, sjekk ut følgende kilder:

- Apple Developer Dokumentasjon: [String](https://developer.apple.com/documentation/swift/string) and [FileHandle](https://developer.apple.com/documentation/foundation/filehandle)
- StackOverflow: [How to read data from a txt file in Swift](https://stackoverflow.com/questions/24581517/read-a-file-url-line-by-line-in-swift)