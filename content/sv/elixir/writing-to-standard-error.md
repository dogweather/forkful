---
title:    "Elixir: Skriva till standardfel"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/elixir/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Varför

Att skriva till standardfel är en viktig del av Elixir-programmering eftersom det tillåter utvecklare att hantera eventuella fel i sina program. Genom att skicka felmeddelanden till standardfel kan utvecklare enkelt identifiera och åtgärda problem som uppstår i koden, vilket resulterar i en mer robust och pålitlig applikation.

## Hur man gör det

För att skriva till standardfel i Elixir, kan du använda funktionen `IO.write_error/2`. Det första argumentet är felmeddelandet som du vill skriva och det andra argumentet är IO-enheten som du vill skriva till, i vårt fall standardfel. Här är ett exempel:

```Elixir
IO.write_error("Det här är ett felmeddelande.", :stderr)
```

När du kör denna kod kommer du att se följande utdata i terminalen:

```shell
Det här är ett felmeddelande.
```

## Djupdykning

Skrivning till standardfel i Elixir är mycket likt att skriva till standardutgång, vilket är en annan funktion som är tillgänglig genom `IO`-modulen. Skillnaden är att standardfel alltid skrivs ut oavsett om standardutgången är omdirigerad eller inte. Detta gör det möjligt för utvecklare att fånga felmeddelanden även när programmet körs av en annan process eller när utdata inte är synlig.

Det är också värt att notera att Elixir tillåter flera IO-enheter att skrivas samtidigt, vilket gör det möjligt att skriva till både standardutgång och standardfel på samma gång. Detta kan vara särskilt användbart när du vill visa utdata för användaren men också logga eventuella fel till standardfel.

## Se även

- [IO-modulen i Elixir-dokumentationen](https://hexdocs.pm/elixir/IO.html)
- [Learn Elixir - Felhantering](https://elixirschool.com/sv/lessons/basics/error-handling/)

Tack för att du läste! Förhoppningsvis har du nu en bättre förståelse för varför och hur man skriver till standardfel i Elixir. Fortsätt lära dig och ha kul med programmering!