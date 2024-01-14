---
title:    "Gleam: Utskrift av felsökningsutdata"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/gleam/printing-debug-output.md"
---

{{< edit_this_page >}}

## Varför

Att skriva kod är som att lösa ett pussel. Ibland kan det vara svårt att förstå varför bitarna inte hamnar på rätt plats. Att använda print-satser för att få ut felmeddelanden eller värden från variabler kan vara otroligt hjälpsamt för att lösa problemet och få en bättre förståelse för koden.

## Hur man gör

Det finns olika sätt att skriva ut debug information i Gleam. Ett sätt är att använda funktionen `debug.print/1`, som tar emot ett argument och skriver ut det i konsolen. Här är ett exempel:

```
Gleam debug.print("Hello world!")
```

Detta kommer att skriva ut "Hello world!" i konsolen när koden körs.

Om du vill skriva ut flera värden eller variabler kan du använda funktionen `debug.fmt/1`, som tar emot en lista av argument och formaterar dem för utskrift. Här är ett exempel på hur man kan använda den:

```
Gleam let
    name = "John"
    age = 30
in
    debug.fmt(["Name: {}", name])
    debug.fmt(["Age: {}", age])
```

Detta kommer att skriva ut följande i konsolen:

```
Name: John
Age: 30
```

## Djupdykning

I Gleam finns det också möjlighet att använda makron för att skriva ut debug-information. Detta kan vara användbart om du bara vill att din debug-kod ska köras i ett visst byggsteg eller miljö. Du kan också använda string interpolation för att enkelt formatera dina utskrifter. Här är ett exempel på hur man kan använda detta:

```
Gleam // #conditional(Prod, debug, {log/2})
        print_me = "I'm being printed!"
        // #string()
        debug ~ "Debugging: {print_me}"
```

I det här exemplet kommer koden att skriva ut värdet av variabeln `print_me` bara om du bygger koden i ett icke-produktionsläge.

## Se även

- [Gleam dokumentation: Debugging](https://gleam.run/book/tour/debugging.html)
- [Gleam dokumentation: Macro Debugging](https://gleam.run/book/tour/macros.html#debugging)
- [Gleam Discord community](https://discord.gg/vWVWBzmMdy)