---
title:    "Gleam: Skriva en textfil"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/gleam/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Varför

Att skriva en textfil är en viktig del av programmering eftersom det ger möjlighet att lagra information som kan användas senare i programmet. Det är också ett sätt att skapa strukturerad och läsbar kod.

## Hur man gör

Att skriva en textfil i Gleam är en enkel process. Först behöver du skapa en variabel som representerar din textfil och sedan använda funktionen "write_file" för att skriva innehållet till filen. Här är ett exempel på hur det kan se ut i Gleam:

```
file := "mina_dagar.txt"
write_file(file, "Måndag")
```

Detta kommer att skapa en textfil med namnet "mina_dagar.txt" och skriva in ordet "Måndag" i filen. Du kan också använda variabler eller andra datastrukturer som innehåll i din textfil.

## Djupdykning

När du skapar en textfil i Gleam, kan du också använda olika funktioner för att manipulera filen. Till exempel kan du använda "read_file" för att läsa innehållet i en befintlig fil, eller "append" för att lägga till mer information till en befintlig fil. Gleam har också stöd för att skriva till olika filformat, som CSV eller JSON.

## Se även

- [Gleam dokumentation - Filsystem](https://gleam.run/docs/standard-library/fs.html)
- [Lär dig Grundläggande Programmering med Gleam (video på svenska)](https://www.youtube.com/watch?v=CPI-TtPZhSE)