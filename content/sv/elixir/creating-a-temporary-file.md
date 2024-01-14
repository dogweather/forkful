---
title:                "Elixir: Skapa en temporär fil"
programming_language: "Elixir"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elixir/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Varför
Att skapa en temporär fil i Elixir kan vara användbart när man behöver lagra data temporärt för att sedan användas eller vidarebearbetas. Det kan också hjälpa till att hålla koden ren och undvika onödiga filer i projektet.

## Hur man gör
Det första steget för att skapa en temporär fil är att importera `:tempfile` biblioteket. Sedan kan man använda `:tempfile.open/2` funktionen för att skapa en temporär fil och spara den i en variabel. Här är ett exempel på hur man skulle kunna göra det:

```elixir
tmp_file = :tempfile.open()
IO.puts(tmp_file.path)

{:ok, "temp_file_1551204519"}
```

Som vi kan se i detta exempel så returneras en tuple där den första delen är en `:ok` atom för att indikera att allt gick bra och den andra delen är filnamnet med ett unikt nummer som identifierar filen.

## Djupdykning
Det finns några olika val man kan göra när man skapar en temporär fil med `:tempfile.open/2`. Man kan till exempel välja vilken mapp filen ska sparas i eller lägga till en prefix eller suffix till filnamnet. Man kan också välja om filen ska skapas i binärt eller textformat genom att lägga till `binary: true` eller `binmode: true` som argument till funktionen.

Det är också viktigt att komma ihåg att när filen inte längre behövs så bör man ta bort den genom att använda `:file.rm/1` funktionen för att undvika att onödiga filer fyller upp hårddisken.

## Se även
- Officiell dokumentation för `:tempfile` biblioteket [här](https://hexdocs.pm/elixir/Tempfile.html)
- Ett blogginlägg om för- och nackdelar med att använda temporära filer [här](https://medium.com/@davecom/rationale-for-temp-file-libraries-c733f946fd35)