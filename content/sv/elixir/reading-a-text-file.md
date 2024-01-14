---
title:    "Elixir: Att läsa en textfil"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## Varför

Att läsa och hantera textfiler är en viktig grundläggande färdighet inom programmering. Genom att läsa textfiler kan man enkelt spara och hämta data för att sedan bearbeta den i sitt program.

## Så här gör du

För att läsa en textfil i Elixir använder man sig av inbyggda funktionen `File.read/1`. Denna funktion tar in en sökväg till den önskade textfilen som argument och returnerar innehållet i filen som en sträng.

```Elixir
inlägg = File.read("blogpost.txt")
IO.puts(inlägg)
```

Detta kodblock läser innehållet i filen "blogpost.txt" och skriver ut det till konsolen.

## Djupdykning

När man läser en textfil är det viktigt att vara medveten om hur filen är kodad. Elixir har inbyggda funktioner för att hantera olika teckenkodningar, såsom `File.read!/2` för att läsa in filen med en specifik kodning. Det är också viktigt att kolla efter eventuella felmeddelanden när man läser en fil, såsom att filen inte existerar eller att man inte har rättigheter att läsa den.

Ett annat användbart verktyg när man läser en textfil är att använda sig av regex (regular expressions) för att söka efter specifika mönster i filen. Detta kan komma till nytta vid exempelvis dataanalys.

## Se även

[File - Elixir Docs](https://hexdocs.pm/elixir/File.html) <br>
[A Gentle Introduction to Elixir Regex](https://elixirschool.com/en/lessons/basics/pattern-matching/#regex) <br>
[Elixir - Läs en fil](https://medium.com/@lavishlife/hur-l%C3%A4ser-man-en-fil-i-elixir-db8dff112f6d)