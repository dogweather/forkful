---
title:    "Swift: Sjekke om en mappe eksisterer"
keywords: ["Swift"]
---

{{< edit_this_page >}}

## Hvorfor

Å sjekke om en mappe eksisterer er en viktig del av Swift-programmering. Dette vil tillate deg å utføre handlinger basert på om en mappe allerede er opprettet eller ikke. I denne bloggposten vil vi gå gjennom hvorfor og hvordan du sjekker om en mappe eksisterer i Swift-programmering.

## Hvordan

Det første du må gjøre er å importere FileManager i koden din. Dette gjøres ved å legge til `import Foundation` øverst i Swift-filen din.

Deretter vil vi bruke FileManager for å sjekke om en mappe eksisterer eller ikke. Dette gjøres ved å bruke metoden `fileExists(atPath:)`, som tar inn en sti til mappen du ønsker å sjekke. Her er et eksempel på hvordan dette kan gjøres:

```Swift
let fileManager = FileManager.default
let path = "/Users/brukernavn/Documents/minMappe"

if fileManager.fileExists(atPath: path) {
    print("Mappen eksisterer allerede")
} else {
    print("Mappen finnes ikke")
}
```

I dette eksempelet først definerer vi en `fileManager`-variabel som bruker standard filbehandleren. Deretter definerer vi en `path`-variabel som representerer stien til mappen vår. Til slutt sjekker vi om mappen eksisterer ved å bruke `fileExists(atPath:)`-metoden og skriver ut en passende melding.

## Deep Dive

Nå som vi har sett på hvordan du kan sjekke om en mappe eksisterer, la oss ta en dypere titt på dette konseptet. Det er viktig å merke seg at `fileExists(atPath:)`-metoden bare sjekker om en mappe eksisterer på den gitte stien, og ikke om det er en mappe eller fil med det navnet. Det kan også hende at metoden returnerer `false` selv om mappen faktisk eksisterer, på grunn av tillatelser eller andre faktorer.

En annen viktig ting å merke seg er at `fileExists(atPath:)`-metoden bare sjekker eksistensen av en mappe på et eksakt sti-nivå. Det betyr at hvis du for eksempel sjekker om en mappe eksisterer på stien `/Users/brukernavn/Documents/minMappe`, vil den returnere `true` selv om det er en mappe med samme navn på en annen sti, som for eksempel `/Users/brukernavn/Documents/minMappe/undermappe`.

## Se også

- [Apple Dokumentasjon om FileManager](https://developer.apple.com/documentation/foundation/filemanager)
- [How to Check If a File or Folder Exists in Swift](https://www.zerotoappstore.com/how-to-check-if-a-file-folder-exists-in-swift/)
- [FileManager Tutorial: How to Check If a File Exists in Swift](https://www.iosapptemplates.com/blog/swift-tutorials/filemanager-check-file-exists-swift)