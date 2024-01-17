---
title:                "Skriver en tekstfil"
html_title:           "Swift: Skriver en tekstfil"
simple_title:         "Skriver en tekstfil"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/swift/writing-a-text-file.md"
---

{{< edit_this_page >}}

Hva & Hvorfor?
Å skrive en tekstfil er rett og slett å lage en digital versjon av et dokument eller en tekst. Programmere gjør dette for å lagre og behandle data på en strukturert måte.

Hvordan:
```Swift
// Åpne en tekstfil
let file = "min_tekstfil.txt"

// Skriv til filen
let tekst = "Dette er min tekst som skal lagres i filen."
try tekst.write(toFile: file, atomically: true, encoding: .utf8)

// Les fra filen
let lagretTekst = try String(contentsOfFile: file, encoding: .utf8)
print(lagretTekst)

// Legg til ny tekst
let nyTekst = "Dette er ny tekst som legges til i filen."
let fileHandle = try FileHandle(forWritingAtPath: file)
fileHandle?.seekToEndOfFile()
fileHandle?.write(nyTekst.data(using: .utf8)!)
fileHandle?.close()

// Nytt innhold i filen:
// Dette er min tekst som skal lagres i filen.
// Dette er ny tekst som legges til i filen.
```

Deep Dive:
Tekstfiler har vært brukt i programmering siden de første datamaskinene ble laget. I tillegg til å lagre tekst, kan de også inneholde annen informasjon som tall og symboler. Alternativet til å skrive til en tekstfil er å lagre data i en database. I Swift, kan du også bruke String-interpolering til å skrive variabler og konstanter direkte til en tekstfil.

See Also:
- [Swift Documentation on Writing Files](https://developer.apple.com/documentation/foundation/filewritingoptions)
- [Difference Between Writing a Text File and Saving Data in a Database](https://www.bbc.co.uk/bitesize/guides/zjb6cj6/revision/1)