---
title:                "Gleam: Skriva till standardfel"
simple_title:         "Skriva till standardfel"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/gleam/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Varför

Att skriva till standard error är en användbar funktion som tillåter program att skicka ut felmeddelanden och andra meddelanden till en särskild utdataström. Det är ett viktigt sätt att informera användaren om eventuella problem som kan uppstå under exekveringen av ett program.

## Hur man gör

För att skriva till standard error i Gleam kan man använda sig av funktionen `std.io` och `stderr` modulen. Här är ett exempel på hur man skulle kunna skriva ett felmeddelande till standard error i Gleam programmeringsspråk:

```Gleam
std.io.fwrite(stderr, "Ett fel har uppstått: %s", [reason])
```

I detta exempel skickar vi ut en felsträng med hjälp av funktionen `std.io.fwrite` till utdataströmmen `stderr`. Med hjälp av formatsträngen `%s` kan vi ange en variabel (`reason`) som innehåller information om felet som har uppstått. Detta är ett enkelt sätt att skriva till standard error, men det finns även andra möjligheter beroende på vad man vill åstadkomma.

Här är ett annat exempel där vi skriver ut flera felmeddelanden till standard error med hjälp av en `for`-loop:

```Gleam
for reason in reasons {
  std.io.fwrite(stderr, "Ett annat fel har uppstått: %s", [reason])
}
```

## Djupdykning

En intressant anledning till att skriva till standard error är att det erbjuder en mer strukturerad utdata jämfört med att enbart skriva ut till terminalen. Genom att skicka ut meddelanden till standard error kan man även använda sig av loggningsverktyg för att samla information om eventuella fel och problem som uppstår i programmet.

En annan fördel är att man kan skriva ut olika typer av meddelanden till standard error för att skilja mellan olika typer av problem. Till exempel kan man använda `std.io.fwrite` för att skriva ut ett varningsmeddelande medan man använder `std.io.fprintf` för att skriva ut ett felmeddelande.

Sammanfattningsvis är möjligheten att skriva till standard error en viktig del av debuggning och felhantering i Gleam programmeringsspråk.

## Se även

- [Officiell Gleam dokumentation för std.io.](https://gleam.run/documentation/standard_library/std.io/)
- [StackOverflow fråga om att skriva till standard error i Gleam.](https://stackoverflow.com/questions/58128012/how-to-write-to-std-err-in-gleam)
- [Gleam Discord-kanal för diskussion om programmering och problem i Gleam.](https://discord.gg/PVWJg4V)