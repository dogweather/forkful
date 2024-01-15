---
title:                "Skriva en textfil"
html_title:           "Gleam: Skriva en textfil"
simple_title:         "Skriva en textfil"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/gleam/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Varför

Att skriva en textfil är ett enkelt och användbart sätt att lagra och dela information med andra. Textfiler kan användas för allt från att spara anteckningar och listor till att skapa kodfiler och dokumentation för ett projekt.

## Så här gör du

För att skriva en textfil i Gleam, behöver du använda funktionen `gleam/io/filesystem.write_file`. Här är ett enkelt exempel på hur du kan använda den:

```Gleam
import gleam/io/filesystem

fn write() {
    let text = "Hej, det här är en textfil!"
    let result = filesystem.write_file("textfil.txt", text)
    case result {
        Ok -> println!("Textfilen har skrivits!")
        Error(e) -> println("Ett fel uppstod: {}", e)
    }
}
```

I detta exempel skapar vi en variabel `text` som innehåller den text som vi vill skriva till textfilen. Sedan använder vi funktionen `write_file` för att skriva innehållet i variabeln till en fil med namnet "textfil.txt". Om allt går bra, kommer vi att se ett meddelande som bekräftar att textfilen har skrivits. Om det uppstår ett fel, kommer vi att se ett felmeddelande istället.

Det finns också andra funktioner som kan användas för att skriva textfiler i Gleam, till exempel `gleam/io/filesystem.append_file` för att lägga till text till en befintlig textfil eller `gleam/io/filesystem.write_bytes` för att skriva binära data till en fil.

## Djupdykning

När vi skriver en textfil, kan vi använda olika teckenkodningar för att bestämma hur tecken i filen ska representeras. Standardteckenkodningen som används i Gleam är UTF-8, vilket är en vanlig teckenkodning som stödjs av de flesta moderna operativsystem och program.

Det är också viktigt att vara medveten om att textfiler har en bestämd slutmarkör som används för att indikera slutet på en rad. Slutmarkören kan variera mellan olika operativsystem, så det kan vara bra att använda funktionen `gleam/io/filesystem.write_system_text_file` istället för `write_file` om du vill att din textfil ska användas på flera olika plattformar.

## Se även

Här är några användbara länkar för att lära dig mer om att skriva textfiler i Gleam:

- [Gleams officiella dokumentation för filsystemmodulen (på engelska)](https://gleam.run/book/standard-library.html#filesystem)
- [En kort handledning om att skriva textfiler i Gleam (på engelska)](https://medium.com/@eliseuvideiramorales/creating-and-reading-files-in-gleam-f572a17be7f3)