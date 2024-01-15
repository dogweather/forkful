---
title:                "Å sjekke om en mappe finnes"
html_title:           "Swift: Å sjekke om en mappe finnes"
simple_title:         "Å sjekke om en mappe finnes"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/swift/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Hvorfor

Å sjekke om en mappe eksisterer er et viktig steg i å lage robust og pålitelig Swift-kode. Det hjelper deg med å håndtere unntak og sørger for at programmet ditt fungerer som forventet.

## Hvordan gjøre det

``` Swift
let fileManager = FileManager.default
let path = "/Users/mitt_navn/Documents"

// Sjekker om mappen eksisterer
if fileManager.fileExists(atPath: path) {
    print("Mappen finnes allerede")
} else {
    print("Mappen finnes ikke")
}
```

Ved hjelp av `FileManager`-klassen kan du sjekke om en mappe eksisterer på en bestemt bane i systemet ditt. Ved å bruke `fileExists`-metoden, kan du enkelt sjekke om mappen eksisterer og deretter gjøre noe med det. I eksempelet ovenfor, hvis mappen eksisterer, vil "Mappen finnes allerede" bli skrevet ut, ellers vil "Mappen finnes ikke" bli skrevet ut.

## Dypdykk

Det er viktig å merke seg at `fileExists`-metoden også kan brukes til å sjekke om en fil eksisterer på en spesifisert bane. Hvis du vil sjekke om en mappe eksisterer på en relativ bane, for eksempel i forhold til gjeldende arbeidsmappe, kan du bruke `fileExists(atPath: relativePath)`-metoden.

Det er også verdt å merke seg at dette er en synkron operasjon, noe som betyr at det kan føre til at appen din fryser mens den utfører sjekken. For å unngå dette, kan du bruke åpne eller løpende filhandlinger som `open` eller `createFile` i stedet.

## Se også

- [FileManager Class Reference](https://developer.apple.com/documentation/foundation/filemanager)
- [Filbehandling i Swift med FileManager](https://www.appcoda.com/swift-filemanager/)
- [Feilhåndtering i Swift](https://cocoacasts.com/how-to-handle-errors-in-swift)