---
title:                "Elixir: Skapa för standardfel"
programming_language: "Elixir"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elixir/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Varför

Att skriva till standarderror är en viktig del av debugging i din Elixir-kod. Genom att skriva felmeddelanden till standarderror får du en bättre förståelse för vad som orsakar problem i din kod.

## Hur man gör

För att skriva till standarderror i din Elixir-kod, använder du funktionen `IO.puts/2`. Det första argumentet är felmeddelandet du vill skriva och det andra argumentet är `:stderr`, som är en atom som talar om att du vill skriva till standarderror istället för standardoutput. Här är ett exempel på hur du skulle använda denna funktion:

```Elixir
IO.puts("Detta är ett felmeddelande", :stderr)
```

Detta skulle skriva ut "Detta är ett felmeddelande" till standarderror.

## Djupdykning

När du skriver till standarderror, är det viktigt att ha i åtanke att ett felmeddelande bara är en bit av information för att hjälpa dig att felsöka din kod. Det är fortfarande ditt ansvar att analysera och förstå varför ett fel uppstår och hur man kan fixa det.

Det kan också vara användbart att använda loggningsverktyget i Elixir, Logger, för att skriva till både standardoutput och standarderror, beroende på typen av information du vill ha. Du kan läsa mer om Logger i Elixirs dokumentation.

## Se även

- [Elixir IO-documentation](https://hexdocs.pm/elixir/IO.html)
- [Elixir Logger-dokumentation](https://hexdocs.pm/logger/Logger.html)
- [Elixir-standardbiblioteket](https://hexdocs.pm/elixir/stdlib.html)