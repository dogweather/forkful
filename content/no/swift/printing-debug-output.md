---
title:                "Utskrift av feilsøkingsresultat"
html_title:           "Arduino: Utskrift av feilsøkingsresultat"
simple_title:         "Utskrift av feilsøkingsresultat"
programming_language: "Swift"
category:             "Swift"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/swift/printing-debug-output.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Utskrift av feilsøkingsdata er måten vi kan se kjøringdataen i applikasjonen vår. Programmerere gjør dette for å løse problemer raskt og effektivt.

## Hvordan:
Her er noen grunnleggende Swift-koder for å vise hvordan vi kan skrive ut feilsøkingsdata. 

```Swift
var name = "Ola"
print("Hei, \(name)!")
```

Dette vil gi:

```
Hei, Ola!
```

Hvis du vil skrive ut feilsøkingsdata kun i en debugsession, bruk `debugPrint` isteden:

```Swift
debugPrint("Dette vil kun vises i feilsøkingsøkter.")
```

## Dypdykk
**Historisk kontekst:** Utskrift av feilsøkingsdata har alltid vært en del av Swift, og dette stammer fra veletablerte praksis innen programmering. 

**Alternativer:** Du kan også bruke `dump` funksjonen for å skrive ut mer detaljerte objektdetaljer:

```Swift
struct Person {
    let name: String
    let age: Int
}

let person = Person(name: "Ola", age: 30)
dump(person)
```

Output: 
```
▿ Person
  - name: "Ola"
  - age: 30
```

**Detaljer om implementering:** Når du bruker `print` eller `debugPrint`, kaller disse funksjonene faktisk `TextOutputStreamable` protokollene for objektene dine. 

## Se Også:
[Apple Documentation: Debugging with Xcode](https://developer.apple.com/library/archive/documentation/ToolsLanguages/Conceptual/Xcode_Overview/DebugYourApp/DebugYourApp.html)
[Swift Documentation: Customizing String Interpolation](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)