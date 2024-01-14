---
title:                "Swift: Å skrive en tekstfil"
simple_title:         "Å skrive en tekstfil"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/swift/writing-a-text-file.md"
---

{{< edit_this_page >}}

# Hvorfor

Å skrive en tekstfil er en viktig ferdighet for enhver Swift-programmerer. Tekstfiler er en enkel måte å lagre og oppbevare data på, spesielt når man trenger å lese og behandle store mengder informasjon.

# Hvordan man gjør det

For å skrive en tekstfil i Swift, må du først opprette en ny fil ved hjelp av FileManager-klassen. Deretter kan du bruke den innebygde funksjonen "```write(toFile:atomically:)``` for å skrive innholdet du vil lagre i filen.

```Swift
let fileManager = FileManager.default
let file = "minTekstfil.txt"

if let dir = fileManager.urls(for: .documentDirectory, in: .userDomainMask).first {

    let fileURL = dir.appendingPathComponent(file)

    let text = "Dette er min første tekstfil!"
    do {
        try text.write(to: fileURL, atomically: false, encoding: .utf8)
    }
    catch {/* error handling */}
}
```

Når du kjører koden, vil den opprette en ny tekstfil med navnet "minTekstfil.txt" og skrive innholdet du har gitt.

# Dypdykk

Det er mange alternativer og funksjoner som kan brukes når det kommer til å skrive en tekstfil i Swift. For eksempel kan du legge til linjeskift ved å bruke "```+ "\n"``` etter innholdet du vil skrive. Du kan også endre skriveberegenskaper, som å legge til en hyphen mellom hvert ord.

# Se også

- [Swift.org](https://swift.org/)
- [Apple Developer Documentation](https://developer.apple.com/documentation/swift)