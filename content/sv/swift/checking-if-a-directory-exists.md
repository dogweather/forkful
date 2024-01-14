---
title:    "Swift: Kontrollera om en mapp finns"
keywords: ["Swift"]
---

{{< edit_this_page >}}

## Varför

Att kontrollera om en mapp finns är en viktig del av att skriva stabil och robust kod. Genom att säkerställa att en mapp finns innan vi använder den kan vi undvika oönskade fel och kraschar i vårt program.

## Hur man gör det

För att kontrollera om en mapp finns i Swift, använder vi oss av FileManager-klassen. När vi skapar ett FileManager-objekt kan vi sedan använda oss av metoden `fileExists(atPath:)` för att kontrollera om en mapp finns eller inte. Här är ett enkelt exempel:

```Swift
let fileManager = FileManager.default

if fileManager.fileExists(atPath: "/Users/User/Desktop/Folder") {
    // Här kan vi utföra våra operationer på mappen
} else {
    // Om mappen inte finns kan vi göra en annan åtgärd eller ge ett felmeddelande
}
```

I det här exemplet använder vi bara en hårdkodad sökväg för att kontrollera om en specifik mapp finns, men du kan också bygga sökvägar dynamiskt baserat på var din app körs eller beroende på användarens input.

## Djupdykning

Det finns några saker att tänka på när du kontrollerar om en mapp finns i Swift. För det första är det viktigt att notera att `fileExists(atPath:)` -metoden bara kontrollerar om det finns en mapp på den angivna sökvägen och inte om den är åtkomlig eller om vår app har tillräckliga behörigheter för att komma åt den. Så även om metoden returnerar true så kan det fortfarande finnas problem med att faktiskt kunna använda mappen.

En annan sak att tänka på är att om du har många mappar som behöver kontrolleras, kan det vara mer effektivt att använda metoden `contentsOfDirectory(atPath:)` som returnerar en array av alla filer och mappar i en specifik mapp. Sedan kan du enkelt loopa igenom arrayen och kontrollera om en specifik mapp finns med hjälp av vanliga stränghanteringsmetoder.

## Se även

- [Apple dokumentation för FileManager](https://developer.apple.com/documentation/foundation/filemanager)
- [Swifty methods for checking file existence](https://useyourloaf.com/blog/quick-guide-to-filemanager/)
- [Stack Overflow tråd om att kontrollera om en mapp finns i Swift](https://stackoverflow.com/questions/29296907/check-if-file-exists-in-swift/29297207#29297207)