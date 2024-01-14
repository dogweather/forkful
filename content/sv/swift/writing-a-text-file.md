---
title:                "Swift: Att skriva en textfil"
simple_title:         "Att skriva en textfil"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/swift/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Varför
Att skriva en textfil är en viktig del av Swift-programmering eftersom det ger möjlighet att lagra och hantera data som behövs för applikationen. Det är också ett sätt att dela och utbyta information mellan olika plattformar och enheter.

## Så här gör du
Först måste du deklarera en variabel som kommer att hålla texten som ska skrivas till filen. Sedan använder du "try/catch" för att försäkra dig om att filen skrivs utan problem. Här är ett exempel på hur du skriver "Hello World!" till en textfil:

```Swift
let text = "Hello World!"

do {
    try text.write(to: URL(fileURLWithPath: "hello.txt"), atomically: false, encoding: .utf8)
    print("Text filen har skapats!")
} catch {
    print("Kunde inte skapa filen: \(error)")
}
```

Efter att du har kört koden kommer en textfil med namnet "hello.txt" att skapas och innehålla texten "Hello World!".

## Djupdykning
För att inte riskera att befintliga textfiler skrivs över, kan du använda "FileManager.default.fileExists" för att kontrollera om filen redan finns innan du skriver till den. Om den inte finns kan du skapa den och skriva till den som i det tidigare exemplet.

Du kan också lägga till fler parametrar i "try" funktionen som "options" och "attributes" för att anpassa hur texten skrivs till filen.

## Se även
- [Apple Developer documentation on writing to and reading from files](https://developer.apple.com/documentation/foundation/archives_and_serialization/writing_files) (Engelska)
- [Swift.org - Files and File System Operations](https://swift.org/documentation/) (Engelska)