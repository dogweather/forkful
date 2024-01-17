---
title:                "Skrivande till standardfel"
html_title:           "Elixir: Skrivande till standardfel"
simple_title:         "Skrivande till standardfel"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elixir/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Skrivning till standard error är en vanlig praxis bland programmerare för att skicka felmeddelanden och loggar till en separat ström från den vanliga utmatningen. Detta gör att utvecklare enkelt kan skilja mellan normala utmatningsmeddelanden och felmeddelanden.

## Såhär gör du:
Det är enkelt att skriva till standard error i Elixir genom användning av `IO.puts/2` funktionen. Detta tar två argument, det första är det meddelande du vill skriva och det andra är strömmen som du vill skriva till - i detta fall `:stderr`. Nedan är ett exempel på hur du kan skriva till standard error i Elixir.

```Elixir
IO.puts("Detta är ett felmeddelande", :stderr)
```

Detta resulterar i att ett felmeddelande skrivs ut på skärmen (eller i terminalen om du kör en Elixir-app):

```bash
Detta är ett felmeddelande
```

## Djupdykning:
Historiskt sett har skrivning till standard error använts för att skicka felmeddelanden och loggar till en specifik ström istället för att blanda dem med den vanliga utmatningen. Det finns dock alternativ till att använda `IO.puts/2` funktionen, som `Logger.error/2` funktionen som är en del av Elixir-standarden, men det är fortfarande vanligt att använda sig av standard error för att skriva ut felmeddelanden.

En viktig implementationsspecifik detalj är att `IO.puts/2` funktionen använder sig av en tryckförbindelse för att skicka utmatningen till strömmen, vilket kan påverka prestandan i situationer där många meddelanden skrivs ut till standard error i en loop.

## Se även:
- Elixir dokumentation om IO.puts/2 funktionen: https://hexdocs.pm/elixir/IO.html#puts/2
- Artiklar om loggning i Elixir: https://www.google.com/search?q=elixir+logging