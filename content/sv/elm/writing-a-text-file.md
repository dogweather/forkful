---
title:                "Skriva en textfil"
html_title:           "Elm: Skriva en textfil"
simple_title:         "Skriva en textfil"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elm/writing-a-text-file.md"
---

{{< edit_this_page >}}

# Vad & Varför?
Att skriva en textfil är, helt enkelt, att lägga in text i en datorfil. Programmerare gör det för att spara eller dela information som inte behöver en avancerad formatering, som till exempel enkel text eller data.

# Så här gör du:
För att skriva en textfil i Elm behöver du först importera modulen "File". Sedan kan du använda funktionen "writeFile" för att skapa filen och skriva in ditt innehåll. Till exempel:

```Elm
import File

writeFile "minTextfil.txt" "Hej världen!"
```

Detta kommer att skapa en fil med namnet "minTextfil.txt" och skriva "Hej världen!" inuti den.

# Djupdykning:
Att skriva textfiler har funnits sedan de första datorerna, och är fortfarande en viktig del av programmering. Det finns olika sätt att skriva textfiler, men Elm-funktioner som "writeFile" gör det enkelt och effektivt att skapa och hantera filer. Om du vill läsa en textfil i Elm kan du använda funktionen "readFile".

# Se även:
- [Elm-dokumentation för "File" modul](https://package.elm-lang.org/packages/elm/file/latest/)
- [Elm-dokumentation för "String" modul](https://package.elm-lang.org/packages/elm/string/latest/) - användbar för att manipulera text innan den skrivs till en fil.