---
title:                "Elixir: Utmatning av felavhjälpning"
simple_title:         "Utmatning av felavhjälpning"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elixir/printing-debug-output.md"
---

{{< edit_this_page >}}

## Varför

Att skriva ut debug-utdata är ett viktigt verktyg för utvecklare att förstå vad som händer i deras kod. Det kan hjälpa till att hitta och åtgärda fel och förstå hur kod utförs i realtid.

## Så här gör du

För att skriva ut debug-utdata i Elixir kan du använda funktionen `IO.inspect/2`. Detta låter dig skriva ut värden och variabler till din terminal som ska hjälpa dig att följa kodens utförande.

```Elixir
IO.inspect(variable)
```

För att skriva ut ett specifikt meddelande kan du använda `IO.puts/2`.

```Elixir
IO.puts("Meddelande")
```

Du kan också använda `IO.inspect/2` för att skriva ut komplexa datastrukturer, som listor och kartor.

```Elixir
list = [1, 2, 3]
IO.inspect(list)
```

Output:

```
[1, 2, 3]
```

För att skriva ut en skapad variabel kan du använda dubbelt citattecken runt variabelnamnet. Detta gör att du kan se det faktiska värdet som skrivs ut istället för att variabelnamnet visas.

```Elixir
variable = "data"
IO.inspect("#{variable}")
```

Output:

```
"data"
```

## Djupdykning

När du använder `IO.inspect/2` för att skriva ut en variabel, returneras också värdet av variabeln. Detta gör att du kan skriva ut variabler mitt i en kodrad utan att bryta ut avkörningen.

För att undvika den här funktionen, kan du använda `IO.debug/2`. Detta skriver bara ut informationen utan att returnera värdet.

En annan användbar funktion för debug-utdata är `IO.puts inspect/2`. Detta låter dig välja vilken detaljnivå du vill ha för dina debug-utdata.

## Se även

- [Elixir Dokumentation: IO-modulen](https://hexdocs.pm/elixir/IO.html)
- [Elixir Dokumentation: IO.inspect/2](https://hexdocs.pm/elixir/IO.html#inspect/2)
- [Elixir Dokumentation: IO.debug/2](https://hexdocs.pm/elixir/IO.html#debug/2)
- [Elixir Dokumentation: IO.puts/2](https://hexdocs.pm/elixir/IO.html#puts/2)