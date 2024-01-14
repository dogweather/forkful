---
title:                "Swift: Å lese en tekstfil"
simple_title:         "Å lese en tekstfil"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/swift/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Hvorfor

Mange programmeringseksperter vil ikke legge for mye vekt på å lese tekstfiler, men å kunne lese tekstfiler kan være svært nyttig for å lagre og behandle store mengder data. Det kan også være en god måte å få tilgang til informasjon som programmet ditt trenger på en enkel måte. Derfor er det viktig å forstå hvordan man kan lese en tekstfil ved hjelp av Swift-programmeringsspråket.

## Hvordan 

For å lese en tekstfil i Swift, må du først angi filstien til tekstfilen ved hjelp av `FileManager` -klassen. Deretter kan du bruke `try? String(contentsOfFile: String, encoding: String.Encoding)`-metoden for å lese filen og lagre den i en variabel.

```Swift 
let fileManager = FileManager.default
let filePath = "tekstfil.txt"

if let contents = try? String(contentsOfFile: filePath, encoding: .utf8) {
    print(contents)
}
```

Dette eksemplet vil skrive ut innholdet i tekstfilen "tekstfil.txt" til konsollen. Husk at du må erstatte filstien med den faktiske plasseringen til tekstfilen på din enhet.

Du kan også lese en tekstfil linje for linje ved hjelp av en `while`-loop og `String`-klassens `init(data: Data, encoding: String.Encoding)`-metode.

```Swift
let fileManager = FileManager.default
let filePath = "tekstfil.txt"

if let data = fileManager.contents(atPath: filePath) {
    if let contents = String(data: data, encoding: .utf8) {
        var lineIndex = 0
        contents.enumerateLines { (line, stop) in
            print("\(lineIndex): \(line)")
            lineIndex += 1
        }
    }
}
```

Dette eksemplet vil skrive ut hver linje i tekstfilen sammen med dens tilhørende linjenummer.

## Deep Dive

Når du leser en tekstfil ved hjelp av Swift, må du også være klar over forskjellige formater og kodingssystemer. For eksempel kan du lese en tekstfil som er kodet som ISO-8859-1, UTF-8 eller UTF-16. Det er også viktig å forstå forskjellen mellom en tekstfil og en binær fil.

En tekstfil består kun av tekst og er enkel å lese og tolke, mens en binær fil kan inneholde annen data og må tolkes på en annen måte. Derfor må du være sikker på at du leser tekstfiler med den riktige metoden og encoding for å unngå uventede feil.

## Se også

* Apple Developer Documentation - [Reading and Writing Files in Swift](https://developer.apple.com/documentation/foundation/filesystem/reading_and_writing_files)
* Ray Wenderlich - [How To Read Text Files In Swift](https://www.raywenderlich.com/1409211-reading-writing-and-interacting-with-files-on-ios-in-swift)
* Swift by Sundell - [Reading and Writing Files in Swift](https://www.swiftbysundell.com/basics/reading-and-writing-files/)