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

## Vad & Varför?
 Att skriva en textfil är ett sätt för programmerare att spara information eller data på ett enkelt och permanent sätt. Det kan vara användbart för att hämta eller skicka data till andra system eller för att spara inställningar och användarinformation.

## Hur:

För att skriva en textfil i Swift finns det flera olika sätt, men ett enkelt sätt är att använda funktionen `write(to:atomically:encoding)` som finns i klassen `NSString`. Detta är ett bra val om du vill spara en textsträng till en fil. Här är ett exempel på hur det kan se ut:

```Swift
let text = "Hej världen!"
let fileName = "textfil.txt"
let fileURL = FileManager.default.urls(for: .documentDirectory, in: .userDomainMask)[0].appendingPathComponent(fileName)

do {
    try text.write(to: fileURL, atomically: true, encoding: .utf8)
    // Om allt går bra så kommer "Hej världen!" att sparas i en textfil som heter "textfil.txt" i dokumentmappen.
    print("Textfilen har sparats!")
} catch {
    // Om det uppstår ett fel kommer koden här att köras istället.
    print("Det gick inte att spara textfilen, försök igen.")
}
```

## Deep Dive:

Historiskt sett har det funnits många sätt att skapa och spara textfiler. I Swift har det nu enkla metoder som `write(to:atomically:encoding)` blivit standard tack vare det enkla syntaxet och den enkla implementationen. Men vissa programmerare väljer fortfarande att använda äldre metoder, som `FileHandle` eller `FileManager`, för att ha mer kontroll över filen.

Det finns också alternativ till att skriva en textfil i Swift, som att spara data i en databas istället för en fil. Detta kan vara bättre i vissa situationer, men det är fortfarande vanligt att använda textfiler för att spara data.

När du skriver en textfil i Swift finns det också flera olika sätt att formatera den på. Den mest vanliga är att använda teckenkodningen `.utf8`, men det finns andra alternativ som `.ascii`, `.utf16` och `.utf32`. Det är viktigt att se till att filen har samma kodning som den data du vill spara för att undvika problem med specialtecken och icke-ASCII-tecken.

## Se också:
- [En tutorial om att skriva och läsa från filer i Swift](https://www.hackingwithswift.com/read/12/2/reading-and-writing-strings-to-a-file)
- [En diskussion om variationer och alternativ till att skriva textfiler i Swift](https://stackoverflow.com/questions/2586747/writing-data-to-a-file-in-swift)