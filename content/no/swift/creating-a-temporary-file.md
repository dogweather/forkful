---
title:                "Oppretting av en midlertidig fil"
html_title:           "Swift: Oppretting av en midlertidig fil"
simple_title:         "Oppretting av en midlertidig fil"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/swift/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Hvorfor
Noen ganger kan det være nødvendig å opprette midlertidige filer i et program. Dette kan være for å lagre data midlertidig mens programmet kjører, eller for å sende data til en annen del av koden. Uansett årsak, er å kunne opprette midlertidige filer en nyttig ferdighet å ha som programmerer.

## Hvordan å opprette en midlertidig fil
For å opprette en midlertidig fil i Swift, kan du bruke NSFileManager-klassen og dens metode `tempFile(withTemplate)`.

```Swift 
let fileManager = FileManager()
let template = URL(fileURLWithPath: NSTemporaryDirectory()).appendingPathComponent("tempFilePrefix").appendingPathExtension("txt").path
do {
    let tempFile = try fileManager.tempFile(withTemplate: template) // opprette en midlertidig fil med forhåndsdefinert prefiks og filtype
    print(tempFile.path) // skriver ut filstien til den midlertidige filen som ble opprettet
} catch {
    print("Kunne ikke opprette midlertidig fil: \(error)") // hvis noe går galt, skrives feilmeldingen ut
}
```

Om alt går som det skal, vil du få følgende output:
```
/var/folders/lb/2xpfjpmj7h3cyyfccgcvvszc0000gn/T/tempFilePrefix3042887085416563608.txt
```

## Dykk dypere
Når du oppretter en midlertidig fil, returneres en URL som peker til den midlertidige filen. Dette betyr at du kan bruke vanlige metoder for å lese og skrive til filen. Husk at den midlertidige filen vil bli slettet så snart programmet avsluttes, så det kan være lurt å kopiere innholdet til en annen fil hvis du ønsker å bevare dataene på en permanent måte.

## Se også
- [Apple Developer Documentation for NSFileManager](https://developer.apple.com/documentation/foundation/nsfilemanager)
- [NSFileManager.tempFile(withTemplate:)](https://developer.apple.com/documentation/foundation/nsfilemanager/1419486-tempfile)