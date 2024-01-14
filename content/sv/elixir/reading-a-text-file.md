---
title:    "Elixir: Läsa en textfil"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/elixir/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Varför

Att läsa in textfiler är en viktig del av många programmeringsuppgifter. Det kan till exempel vara för att bearbeta data eller för att hitta specifika värden. Genom att läsa in en textfil kan vi enkelt få åtkomst till dess innehåll och använda det på olika sätt.

## Så här gör du

För att läsa in en textfil i Elixir använder vi funktionen `File.read!/1`. Denna funktion tar en filväg som argument och returnerar innehållet i filen som en sträng. Låt oss ta en titt på ett exempel:

```Elixir
file_content = File.read!("textfil.txt")
IO.puts file_content
```

I detta exempel läser vi innehållet i filen "textfil.txt" och skriver ut det till konsolen med hjälp av `IO.puts`. Om innehållet i filen är "Hej, världen!" kommer output att se ut som följande:

```
Hej, världen!
```

Detta är en enkel metod för att läsa in en textfil, men det finns flera andra funktioner som kan användas för att läsa in filer på olika sätt. Det är också viktigt att notera att `File.read!/1` returnerar en sträng, så om filen innehåller annan data som inte kan tolkas som en sträng, måste vi använda andra metoder för att läsa in den.

## Djupdykning

För att läsa in textfiler på ett mer flexibelt sätt, kan vi använda funktionen `File.stream!/2`. Denna funktion tar en filväg och en lista av läsbeskrivningar som argument. Låt oss se på ett exempel:

```Elixir
File.stream!("textfil.txt", [:line, :raw]) 
|> Enum.each(fn line -> IO.puts line end)
```

Här använder vi `File.stream!/2` för att läsa in filen "textfil.txt" rad för rad och sedan skriva ut varje rad med hjälp av `Enum.each`. Genom att använda olika läsbeskrivningar (t.ex. `:line` eller `:raw`) kan vi välja hur filen ska läsas in och hur datan ska hanteras.

## Se även

- [Elixir - File module](https://hexdocs.pm/elixir/File.html)
- [Elixir School - Reading and Writing Files](https://elixirschool.com/en/lessons/basics/io/#reading-and-writing-files)