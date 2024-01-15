---
title:                "Skriva en textfil"
html_title:           "Swift: Skriva en textfil"
simple_title:         "Skriva en textfil"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/swift/writing-a-text-file.md"
---

{{< edit_this_page >}}

(# Swift och Skrivande Av Textfiler

Swift är ett populärt programspråk som används för att utveckla mobilappar och webbapplikationer. Det har också en mängd olika funktioner som gör det möjligt att hantera data och filer på en enkel och effektiv sätt. I denna artikel kommer vi att titta på hur man skriver en textfil i Swift på ett enkelt och okomplicerat sätt.

## Varför

Att skriva en textfil kan vara användbart av flera olika skäl. Det kan vara ett sätt att spara data, till exempel användarinformation eller appinställningar. Det kan också vara ett sätt att dela data med andra användare eller program. Oavsett anledning, är det enkelt att skriva en textfil i Swift.

## Så här gör du

För att börja skriva en textfil i Swift behöver du först skapa en ny tom fil. Detta kan du göra genom att använda `FileManager` klassen.

```
let fileManager = FileManager.default
let file = "myFile.txt"

if let directoryURL = fileManager.urls(for: .documentDirectory, in: .userDomainMask).first {
    let fileURL = directoryURL.appendingPathComponent(file)
}
```

Här skapar vi en ny instans av `FileManager` klassen och sedan ett filnamn för vår textfil. Sedan använder vi `urls(for:in:)` metoden för att få tillgång till dokumentmappen för vår app. Slutligen använder vi `appendingPathComponent` metoden för att skapa en `URL` för vår fil.

Efter att vi har skapat denna `URL`, kan vi använda `write` metoden från `String` klassen och skriva till vår fil.

```
let text = "Det här är en text som vi vill skriva till vår fil"

do {
    try text.write(to: fileURL, atomically: false, encoding: .utf8)
} catch {
    print("Kunde inte skriva till filen.")
}
```

Här använder vi `write` metoden tillsammans med `atomically` och `encoding` argument för att indikera att vi inte vill att filen ska skrivas atomiskt och att vi vill använda UTF8-kodning. Om allt går väl kommer vår text att sparas i vår fil på den angivna `URL`.

## Djupdykning

Utöver att skriva till en fil, kan vi också använda `String` klassens `read` metod för att läsa innehållet i en fil.

```
do {
    let text = try String(contentsOf: fileURL, encoding: .utf8)
    print(text)
} catch {
    print("Kunde inte läsa filen.")
}
```

Här använder vi `read` metoden tillsammans med `contentsOf` argumentet för att läsa innehållet i vår fil och spara det som en `String`. Vi kan också använda `append` metoden för att lägga till mer text till vår fil istället för att skriva över allt befintligt innehåll.

Det finns också andra sätt att skriva och läsa textfiler i Swift med hjälp av olika klasser och metoder, så som `OutputStream` och `InputStream` för större filer eller när vi vill hantera filen bit för bit.

## Se också 

- [Apple's dokumentation om FileManager](https://developer.apple.com/documentation/foundation/filemanager)
- [Swift för Nybörjare](https://swift.org/getting-started/)
- [Enkel Swift tutorial från Apple](https://docs.swift.org/swift-book/GuidedTour/GuidedTour.html)