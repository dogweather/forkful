---
title:    "Swift: Sjekke om en mappe eksisterer"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/swift/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

##Hvorfor

Å sjekke om en mappe eksisterer er en viktig del av Swift-programmering. Dette er spesielt nyttig når du prøver å lese eller lagre filer på en bestemt sti i mappen.

##Slik Gjør Du Det

For å sjekke om en mappe eksisterer i Swift, kan du bruke FileManager-klassen. Her er en enkel kode for å sjekke om mappen "Dokumenter" eksisterer:

```Swift
// Sjekker om Dokumenter-mappen eksisterer
let fileManager = FileManager.default
if fileManager.fileExists(atPath: "/Users/brukernavn/Documents") {
    print("Dokumenter-mappen eksisterer.")
} else {
    print("Dokumenter-mappen eksisterer ikke.")
}
```

Output:
```Swift
Dokumenter-mappen eksisterer.
```

Det er også mulig å sjekke om en mappe eksisterer ved å bruke URL istedenfor bane. Her er en kode for å sjekke om mappen "Bilder" eksisterer på skrivebordet:

```Swift
let desktop = FileManager.default.urls(for: .desktopDirectory, in: .userDomainMask).first!
let picturesFolder = desktop.appendingPathComponent("Bilder")
if fileManager.fileExists(atPath: picturesFolder.path) {
    print("Bilder-mappen eksisterer.")
} else {
    print("Bilder-mappen eksisterer ikke.")
}
```

Output:
```Swift
Bilder-mappen eksisterer ikke.
```

##Dypdykk

Når du sjekker om en mappe eksisterer, er det viktig å merke seg at det bare sjekker om selve mappen eksisterer, ikke om den har noen filer eller undermapper i seg. En annen ting å merke seg er at det kan være forskjellige måter å skrive banen til mappen på avhengig av systemet ditt.

For å sikre at koden din fungerer på alle plattformer, kan du bruke FileManager.default.urls-metoden som vist i eksemplene ovenfor.

##Se Også

- [FileManager dokumentsasjon](https://developer.apple.com/documentation/foundation/filemanager)
- [Swift - Finne stier og baner](https://www.hackingwithswift.com/example-code/strings/how-to-find-the-paths-between-two-directories)
- [Swift - How to check if file exists](https://www.codementor.io/@winnieliang/file-handling-in-swift-part-1-working-with-files-and-directories-2zqkzp85s)