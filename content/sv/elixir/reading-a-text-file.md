---
title:                "Elixir: Läsning av en textfil"
simple_title:         "Läsning av en textfil"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elixir/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Varför

Att läsa en textfil kan vara en viktig uppgift i en Elixir-applikation. Genom att kunna läsa data från en fil kan vi få tillgång till information som är viktigt för vår applikation. Det kan vara allt från användardata till konfigurationsinställningar.

## Hur

För att läsa en textfil i Elixir, används funktionen File.read/1. Den tar en filväg som argument och returnerar en tuple med resultatet. Här är ett exempel på hur vi skulle kunna läsa en textfil som innehåller namn på användare och deras ålder:

```Elixir
contents = File.read("anvandare.txt")

IO.puts("Användare:")
io.puts(contents)
```

Detta kodexempel öppnar filen "anvandare.txt" och tilldelar innehållet i filen till en variabel. Sedan printar det ut innehållet i filen i terminalen. Om vi antar att innehållet i "anvandare.txt" är:

```
Lisa, 25
Pelle, 30
```

Skulle outputen bli:

```
Användare:
Lisa, 25
Pelle, 30
```

Det finns också andra funktioner som kan användas för att läsa en textfil, som File.read!/1 som kastar ett error om filen inte hittas och File.stream!/1 som skapar en ström av data från filen. Genom att läsa på om olika filrelaterade funktioner i Elixir, kan vi hitta den som passar bäst för våra specifika behov.

## Djupdykning

När vi läser en textfil i Elixir, är filen inte direkt läsbar för vår applikation. Istället blir filens innehåll en binärsträng, vilket kräver att vi hanterar datan på rätt sätt för att kunna använda den. Ofta måste vi omvandla binärsträngen till en lista av strängar för att få utdata i rätt format.

Det är också viktigt att hantera eventuella fel som kan uppstå vid läsning av filen, som till exempel att filen inte finns eller att den har felaktigt formaterade data. Genom att använda funktioner som File.read!/1 eller att inkludera felhanteringslogik i vår kod, kan vi säkerställa att vår applikation hanterar dessa fall på ett bra sätt.

## Se även

- [Filhantering i Elixir](https://elixir-lang.org/getting-started/io-and-the-file-system.html)
- [Dokumentation för File-modulen](https://hexdocs.pm/elixir/File.html)
- [Elixir för nybörjare](https://www.youtube.com/watch?v=u0lboLNVDNk) (på svenska)