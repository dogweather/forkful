---
title:                "Skriva en textfil"
html_title:           "Elixir: Skriva en textfil"
simple_title:         "Skriva en textfil"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elixir/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att skriva en textfil innebär att man sparar information i en fil, till exempel i form av text. Det kan vara användbart när man vill spara data för senare användning eller när man vill dela information med andra. Programmerare skriver textfiler för att spara kod eller data som deras program behöver för att fungera korrekt.

## Hur man gör:
Skriva en textfil i Elixir är enkelt. Du kan använda ```File.write/2``` funktionen för att skapa en ny textfil och skriva innehållet du vill spara. Du kan också använda ```File.append/2``` funktionen för att lägga till nya rader i en befintlig textfil.

Exempel:
```Elixir
File.write("min_textfil.txt", "Detta är en textfil som innehåller viktig information.")
```
```Elixir
File.append("min_textfil.txt", "\nHär är en till rad som läggs till i filen.")
```

Resultat (innehåll i min_textfil.txt):
```
Detta är en textfil som innehåller viktig information.
Här är en till rad som läggs till i filen.
```

## Djupdykning:
Historiskt sett har textfiler varit ett vanligt sätt att spara och dela information på inom programmering. Det finns många olika filformat för text, men de vanligaste är .txt, .csv och .json. Alternativt kan programmerare också använda databaser för att lagra information istället för textfiler.

I Elixir är det möjligt att skriva textfiler på olika sätt, till exempel med hjälp av inbyggda funktioner som ```File.write/2``` och ```File.append/2```, eller genom att använda en tredjepartsbibliotek som ```Elixir-CSV``` för att skriva till .csv filer.

## Se även:
- [Elixir Documentation on File Module](https://hexdocs.pm/elixir/File.html)