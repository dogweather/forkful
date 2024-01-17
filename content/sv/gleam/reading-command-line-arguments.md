---
title:                "Läsning av kommandoradsargument"
html_title:           "Gleam: Läsning av kommandoradsargument"
simple_title:         "Läsning av kommandoradsargument"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/gleam/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att läsa in kommandoradsargument är ett vanligt förekommande koncept inom programmering. Det innebär helt enkelt att man tar emot värden som användaren matar in i ett program via terminalen eller kommandoraden. Detta är en användbar funktion då det tillåter programmet att interagera med användarna och ge dem en ökad flexibilitet och kontroll över hur programmet körs.

## Hur man:
För att läsa in kommandoradsargument i Gleam kan man använda funktionen `gleam/core/io.read_args()` som tar emot en lista av strängar från användaren. Här är ett exempel på hur man kan använda den:

```Gleam
import gleam/core/io

fn main() {
    let args = io.read_args()
    io.print("Du matade in följande argument: #{args}")
}
```

Om vi nu kör detta program med kommandoradsargumenten "hej", "världen", kommer outputen att vara "Du matade in följande argument: [hej, världen]". Det går även att ange kommandoradsargument vid kompileringen av programmet, vilket kan vara användbart vid mer avancerade program.

## Djupdykning:
Att läsa in kommandoradsargument är ett viktigt koncept inom programmering och används ofta för att ge användare en större kontroll över hur ett program körs. Det finns även andra sätt att ta emot input från användaren, som t.ex. via en GUI (grafiskt användargränssnitt). Men att läsa in kommandoradsargument är en snabb och enkel metod som passar väl för många situationer.

## Se även:
För mer information om hur man hanterar kommandoradsargument i Gleam, se dokumentationen för `gleam/core/io` modulen: [länk till dokumentationen](https://gleam.run/modules/gleam_core/io.html#read_args).