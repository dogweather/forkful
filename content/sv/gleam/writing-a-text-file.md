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

## Vad & Varför?
Att skriva en textfil är en vanlig uppgift för programmerare. Det innebär att skriva en sekvens av tecken till en fil, som sedan kan läsas och tolkas av en dator. Programmare gör detta för att spara data eller resultat från sina program, för att dela med sig av sin kod eller för att ha en backup av sin kod.

## Så här gör du:
För att skriva en textfil i Gleam behöver du använda modulen `gleam/io`. Den innehåller funktionen `write_text_file` som tar emot två argument - en sträng med filnamnet och en sträng med innehållet du vill skriva. Här är ett exempel:

```Gleam
import gleam/io

fn main() {
    let filename = "min_fil.txt"
    let content = "Hej världen!"
    let result = write_text_file(filename, content)
    case result {
        Ok(_) -> println("Texten har skrivits till filen.")
    }
}
```

## Djupdykning:
Att skriva en textfil är inte bara uppgift för Gleam-programmerare - det är en vanlig uppgift för alla programmeringsspråk. Ett alternativ till att använda `gleam/io` är att använda en specifik modul för att skriva textfiler, till exempel `std/fs` i Språket, eller att använda standardmetoden `FileWriter` i Java. Det är också värt att notera att det finns flera olika sätt att öppna och skriva till en fil i Gleam, beroende på vad man är van vid eller bekväm med.

## Se även:
- [Inför IO i Språket](https://spektrakel.de/2009/11/24/io-file-handling-in-språket/)
- [Java FileWriter guide](https://www.w3schools.com/java/java_files_create.asp)