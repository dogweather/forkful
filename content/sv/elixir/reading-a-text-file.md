---
title:                "Läsa en textfil"
html_title:           "Elixir: Läsa en textfil"
simple_title:         "Läsa en textfil"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elixir/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Varför

Att läsa en textfil är en viktig del av många Elixir-program. Det kan användas för att läsa in data, konfigurationsfiler eller för att manipulera stora mängder data. Genom att läsa en textfil kan du enkelt få åtkomst till strukturerad data för att bearbeta den på ett effektivt sätt.

## Hur du gör det

För att läsa en textfil i Elixir använder du funktionen `File.stream!` tillsammans med `Enum.each`. Detta kommer att läsa in filen rad för rad och låter dig utföra olika åtgärder på varje rad. Använd `IO.puts` för att skriva ut varje rad till terminalen.

```Elixir
File.stream!("mina_fil.txt") 
|> Enum.each(&IO.puts/1)
```

Resultatet blir att varje rad i filen skrivs ut på skärmen.

## Utökad förklaring

För att läsa en textfil på ett mer detaljerat sätt, kan du använda funktionerna `File.open!/2` och `IO.read/2`. `File.open!/2` öppnar en fil för läsning och `IO.read/2` läser data från filen. Det är viktigt att nämna att `File.open!/2` returnerar en "fil" struktur vilket betyder att den måste stängas med `File.close/1` när den är klar.

```Elixir
file = File.open!("mina_fil.txt")
IO.read(file, :line) # läser filen rad för rad
```

Denna metod ger dig mer kontroll över hur filinnehållet hanteras och kan vara användbart för mer avancerade användningsfall.

## Se också

- [Elixir File modul](https://hexdocs.pm/elixir/File.html)
- [Läsa filinnehåll rad för rad](https://stackoverflow.com/questions/21042082/read-a-file-line-by-line-in-elixir)
- [IO modulen i Elixir](https://hexdocs.pm/elixir/IO.html)