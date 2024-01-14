---
title:    "Elixir: Att skapa en tillfällig fil"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/elixir/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Varför

Att skapa en temporär fil kan vara en användbar funktion när man arbetar med Elixir. Det kan hjälpa till att organisera och hantera viss data eller tillfälliga filer under utförandet av ett program.

## Hur man gör

Det finns flera olika sätt att skapa en temporär fil i Elixir. En möjlig metod är att använda funktionen `Tempfile.open()` från biblioteket `:stdlib`. Nedan följer ett exempel på hur man kan använda denna funktion:

```Elixir
{:ok, file} = Tempfile.open("myfile.txt")
IO.puts("Skapade en temporär fil vid sökvägen: #{file.path}")
```

Output av detta exempel skulle vara: `Skapade en temporär fil vid sökvägen: /tmp/myfile20190520-12345-18we9lu.txt`

## Djupdykning

När man skapar en temporär fil med `Tempfile.open()` så skapas filen i operativsystemets standardtempmapp, vilket i de flesta fall är `/tmp`. Det finns dock möjlighet att specificera en annan sökväg som argument till funktionen om man önskar det.

Denna metod av att skapa en temporär fil är dock inte garanterad att fungera på alla operativsystem. Om man vill ha en mer pålitlig metod så kan man istället använda biblioteket `elan`.

## Se även

- [Elixir Tempfile Documentation](https://hexdocs.pm/elixir/Tempfile.html)
- [Elixir Stdlib Documentation](https://hexdocs.pm/elixir/stdlib.html)
- [Elixir Elan Documentation](https://github.com/elpunkt/elan)