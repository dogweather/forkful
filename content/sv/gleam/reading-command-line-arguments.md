---
title:                "Gleam: Läsning av kommandoradsargument"
programming_language: "Gleam"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/gleam/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Varför
Att läsa kommandoradsargument kan vara en viktig del av att skriva effektiva och användarvänliga Gleam-program. Genom att läsa kommandoradsargument kan du ge din användare möjlighet att anpassa programmet efter deras specifika behov och preferenser. Detta gör också att programmet blir mer mångsidigt och användbart för en bredare publik.

## Hur man gör det
För att läsa kommandoradsargument i Gleam så behöver du använda funktionen `CommandLine.arguments()` som returnerar en lista av strängar. Dessa strängar representerar de argument som användaren har skrivit in när de startar programmet.

Här är ett exempel på hur man kan använda `CommandLine.arguments()` i ett Gleam-program:

```Gleam
function main(_arguments) {
    arguments |> List.iter(print)
}
```
I det här exemplet använder vi `List.iter()` för att iterera igenom listan av argument och skriva ut dem med `print()` funktionen. Om vi skulle köra detta program från kommandoraden med argumenten `hello world` så skulle output bli:

```
hello
world
```
Detta visar att vi enkelt kan få tillgång till och använda kommandoradsargument i vårt Gleam-program.

## Fördjupning
När du läser kommandoradsargument så finns det även möjlighet att göra det mer robust genom att använda en tredjepartsbibliotek som till exempel [Commando](https://github.com/RefugeesWelcome/gleam-commando). Detta kan hjälpa dig att hantera felaktiga eller ogiltiga argument och ge en bättre användarupplevelse.

Det är också värt att nämna att `CommandLine.arguments()` endast ger åtkomst till de argument som användaren skriver in när de startar programmet. Om du vill ha möjlighet att läsa argument under själva körningen av programmet så kan du använda [Environment](https://github.com/gleam-lang/environment) biblioteket.

## Se även
- [Gleam Commando](https://github.com/RefugeesWelcome/gleam-commando)
- [Gleam Environment](https://github.com/gleam-lang/environment)

Vi hoppas att denna guide har hjälpt dig att förstå hur du kan läsa kommandoradsargument i Gleam och hur det kan hjälpa dig att skapa mer anpassningsbara och användbara program. Lycka till med din programmering!