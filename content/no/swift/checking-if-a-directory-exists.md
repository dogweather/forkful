---
title:                "Sjekke om en mappe eksisterer"
html_title:           "Swift: Sjekke om en mappe eksisterer"
simple_title:         "Sjekke om en mappe eksisterer"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/swift/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Sjekking om en mappe eksisterer er når en programmerer sjekker om en spesifisert mappe finnes på et gitt sted i programmet. Dette gjøres for å sikre at endringer i filstrukturen eller plassering ikke ødelegger funksjonaliteten til programmet.

## Hvordan:

Sjekke om en mappe eksisterer kan gjøres ved hjelp av Swifts File Manager bibliotek. Her er et eksempel på hvordan det kan gjøres:

```Swift
let fileManager = FileManager.default
let directoryURL = fileManager.urls(for: .documentDirectory, in: .userDomainMask).first // velger ønsket mappe å sjekke
if let url = directoryURL {
    do {
        let results = try fileManager.contentsOfDirectory(at: url, includingPropertiesForKeys: nil) //sjekker om mappen har noen innhold
        print("Mappen finnes og har følgende filer: ")
        print(results)
    } catch {
        print("Mappen eksisterer, men har ingen filer i seg")
    }
} else {
    print("Mappen finnes ikke")
}
```

Output vil se slik ut dersom mappen eksisterer og har filer i seg:

```Swift
Mappen finnes og har følgende filer:
[file:///Users/bruker/Documents/file1.pdf, file:///Users/bruker/Documents/file2.docx, file:///Users/bruker/Documents/file3.png]
```

Og output vil se slik ut dersom mappen eksisterer, men ikke har noen filer i seg:

```Swift
Mappen eksisterer, men har ingen filer i seg
```

## Dypdykk:

Sjekking om en mappe eksisterer ble introdusert i LSE (Large-Scale Environment) for IBM i systemer på 80-tallet. En alternativ måte å sjekke om en mappe eksisterer på er ved hjelp av systemkall, men dette kan være mer komplekst og kreve mer kode. Selv om Swifts File Manager bibliotek er det enkleste og mest effektive metoden for å sjekke om en mappe eksisterer.

## Se også:

For mer informasjon om Swifts File Manager bibliotek og andre metoder for å sjekke om mapper eksisterer, se disse lenkene:

- [Apple Developer Documentation](https://developer.apple.com/documentation/foundation/filemanager)
- [Alternative methods for checking if a directory exists in Swift](https://www.hackingwithswift.com/example-code/system/how-to-check-whether-a-file-or-directory-exists-using-filemanager)