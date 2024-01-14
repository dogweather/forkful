---
title:                "Swift: Opprette en midlertidig fil"
simple_title:         "Opprette en midlertidig fil"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/swift/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Hvorfor

Å lage midlertidige filer er en vanlig praksis i programmmering for å midlertidig lagre data eller informasjon. Dette kan være nyttig når du trenger å teste koden din eller utføre en midlertidig oppgave.

## Hvordan

For å lage en midlertidig fil i Swift, kan du bruke `FileManager`-klassen og dens `createFile`-metode. Her er et eksempel på hvordan du kan opprette en midlertidig fil:

```Swift
let fileManager = FileManager.default
let temporaryDir = NSTemporaryDirectory()
let temporaryFile = temporaryDir.appending("myTempFile.txt")

if fileManager.createFile(atPath: temporaryFile, contents: nil, attributes: nil) {
    print("Midlertidig fil opprettet!")
} else {
    print("Noe gikk galt under opprettelsen av den midlertidige filen.")
}
```

I dette eksempelet bruker vi `NSTemporaryDirectory()` for å få den midlertidige mappen på enheten vår, og vi oppretter en sti til vår midlertidige fil ved å kombinere stien med filnavnet vi ønsker. Deretter bruker vi `createFile`-metoden for å faktisk lage filen, og sjekker om det var vellykket. Hvis alt gikk som planlagt, skal vi få en bekreftelse i terminalen.

## Dypdykk

Nå som vi har sett på et enkelt eksempel på hvordan vi kan opprette en midlertidig fil, la oss ta en nærmere titt på hva som skjer bak kulissene. Når vi kaller `createFile`, vil det automatisk opprette en tom fil på den angitte plasseringen. Hvis vi ønsker å legge til innhold i filen vår, kan vi gjøre det ved å spesifisere `contents`-parameteren med ønsket data i form av en `Data`-objekt.

En annen viktig ting å merke seg er at når appen vår avsluttes, vil alle midlertidige filer som er opprettet ved hjelp av `createFile` bli automatisk slettet. Dette er fordi disse filene befinner seg i en spesiell midlertidig mappe på enheten vår.

## Se også

Her er noen nyttige ressurser for å lære mer om å lage midlertidige filer i Swift:

- [Apple Documentation - FileManager](https://developer.apple.com/documentation/foundation/filemanager)
- [Apple Developer Video - Temporary Files](https://developer.apple.com/videos/play/wwdc2020/10111/)
- [Hvordan lage midlertidige filer i Swift](https://www.appcoda.com/temporary-files-swift/)