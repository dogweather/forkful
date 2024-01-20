---
title:                "Opprette en midlertidig fil"
html_title:           "Bash: Opprette en midlertidig fil"
simple_title:         "Opprette en midlertidig fil"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/swift/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å lage en midlertidig fil er prosessen der en programmerer skaper en fil som er ment for kortvarig bruk. Dette gjøres for problemløsing, midlertidig lagring av data eller for å ta backup av informasjon mens en jobb utføres.

## Hvordan:
Her er en kodeeksempel for å lage en midlertidig fil i Swift:

```Swift
import Foundation

let tempDirectoryURL = NSURL.fileURL(withPath: NSTemporaryDirectory(), isDirectory: true)
let tempFileURL = tempDirectoryURL.appendingPathComponent(UUID().uuidString)

do {
    try "Midlertidig data".write(to: tempFileURL, atomically: true, encoding: .utf8)
} 
catch {
    print("Det oppstod en feil: \(error)")
}
```
Utskriften vil ikke vise noe som er veldig spesifikt da UUID().uuidString genererer en unik streng hver gang koden kjøres. Det er faktisk den unike strengen som danner navnet på den midlertidige filen.

## Dypdykk:
Historisk sett har midlertidige filer blitt brukt av programmer siden tidlig i datamaskinens liv, og er fortsatt en kritisk del av de fleste systemer, spesielt når det kommer til feilsøking og dataintegritet. 

Det er mange måte å lage midlertidige filer på. For eksempel kan det brukes biblioteker som FileManager, eller metoder som tmpfile(). I Swift foretrekkes NSTemporaryDirectory metoden, grunnet dens enkelhet og sikkerhet.

Skapingen av midlertidige filer i Swift er en enkel og sikker affære. NSURL.fileURL(withPath: NSTemporaryDirectory(), isDirectory: true) koden brukes til å få tilgang til midlertidige kataloger, mens UUID().uuidString brukes for å sikre at filen har et unikt navn. Feilhåndtering blir brukt gjennom Swift sine do-try-catch blokker.

## Se Også:
2. Swift Documentation: [Error Handling](https://docs.swift.org/swift-book/LanguageGuide/ErrorHandling.html)
3. Stack Overflow: [How to create a temporary directory/folder in Swift?](https://stackoverflow.com/questions/37401342/how-to-create-a-temporary-directory-folder-in-swift)
4. UUID: [UUID in Swift](https://developer.apple.com/documentation/foundation/uuid)