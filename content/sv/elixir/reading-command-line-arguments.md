---
title:                "Läsa kommandoradsargument"
html_title:           "Elixir: Läsa kommandoradsargument"
simple_title:         "Läsa kommandoradsargument"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elixir/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

Det finns flera olika sätt att läsa in kommandoradsargument i Elixir, men varför skulle man vilja göra det? Det finns faktiskt flera prakiska tillämpningar för att läsa in argument från terminalen, såsom att köra ett specifikt script med olika parametrar eller att skriva ett program som interagerar med andra program via kommandoraden.

## Varför

Det är alltid bra att få feedback och input från användare i realtid, och genom att läsa in kommandoradsargument kan man göra just det. Med Elixir kan man enkelt använda ett inbyggt bibliotek för att läsa in argument från terminalen och sedan använda dem i sitt program.

## Hur man gör

För att läsa in kommandoradsargument i Elixir behöver man använda funktionen `System.argv/0`, som returnerar en lista med alla argument som skickats till programmet via terminalen. Man kan sedan loopa igenom listan och använda argumenten som man vill. Detta kan se ut såhär:

```Elixir
# Läs in kommandoradsargument
arguments = System.argv()

# Loopa igenom alla argument och skriv ut dem
for arg <- arguments do
  IO.puts("Argument " <> arg)
end
```

Om man exempelvis kör programmet med kommandot `elixir program.ex arg1 arg2 arg3`, kommer outputen att bli:

```
Argument arg1
Argument arg2
Argument arg3
```

## Djupdykning

För de som vill lära sig mer om hur man hanterar kommandoradsargument i Elixir, finns det flera olika sätt att optimera och strukturera koden på. Man kan till exempel använda sig av mönstermatchning för att hantera olika typer av argument på ett mer effektivt sätt, eller använda sig av Elixirs inbyggda `OptionParser` för att lägga till flaggor och flaggvärden till sina argument.

Det kan också vara användbart att veta att man kan sätta default-värden för argument som inte skickas med från terminalen, genom att använda funktionen `System.get_env/2` för att hämta värden från miljövariabler.

## Se även

- [Elixirs officiella dokumentation för `System`-modulen](https://hexdocs.pm/elixir/System.html)
- [En guide för hantering av kommandoradsargument i Elixir](https://www.learnelixir.tv/episodes/Episode-019-Passing-arguments-from-the-command-line)