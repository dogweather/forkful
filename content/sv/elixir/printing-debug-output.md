---
title:    "Elixir: Utmatning av felanalys"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/elixir/printing-debug-output.md"
---

{{< edit_this_page >}}

## Varför

När vi programmerar är det ofta nödvändigt att felsöka vår kod och se hur den beter sig i olika situationer. En enkel och effektiv metod för att göra detta är att använda sig av debug output, eller utskrifter av information om variabler eller steg i ett program. Detta kan hjälpa oss att förstå vad som händer i koden och hitta eventuella felaktigheter.

## Hur man gör det

För att skriva ut debug output i Elixir kan du använda dig av funktionen `IO.inspect/2`. Denna funktion tar två argument, det första är det du vill skriva ut och det andra är en lista av alternativa konfigurationer. Om du till exempel vill skriva ut värdet av en variabel `x` kan du använda följande syntax:

```Elixir
IO.inspect(x)
```

Du kan också skriva ut flera variabler samtidigt genom att separera dem med kommatecken:

```Elixir
IO.inspect(variable1, variable2)
```

Om du vill ange ytterligare konfigurationer, som till exempel att skriva ut objektets klass eller visa stackspåret för en funktion, kan du göra det genom att lägga till det som en lista av argument efter den sista variabeln. Här är ett exempel på en mer avancerad `IO.inspect`:

```Elixir
IO.inspect(data, labels: [data_type: :element, pid: :self])
```

Detta kommer att skriva ut värdet av `data` samt dess klass och process-id.

## Djupdykning

För att göra debug output ännu mer användbart kan du också använda dig av funktionen `IO.puts/2`, som skriver ut en sträng till terminalen. Detta kan vara till hjälp för att markera specifika steg i koden eller skriva ut anpassade meddelanden. Du kan också använda `IO.inspect` för att skriva ut specifika delar av datastrukturer, som till exempel värden i en lista eller ett objekts attribut.

Det är också möjligt att använda sig av modulen `Logger` för att skriva ut debug information. Detta ger dig möjlighet att ange olika nivåer av loggning beroende på hur allvarligt felet är. Du kan använda `debug`-nivån för att skriva ut specifik information under utveckling och sedan byta till `error`-nivån för att bara skriva ut viktig information vid produktion.

## Se även

För mer information om andra användbara funktioner och moduler inom Elixir, kan du kolla in följande resurser:

- [Elixir Language website](https://elixir-lang.org) (officiell Elixir-sida)
- [Elixir School](https://elixirschool.com/sv/) (en interaktiv plattform för att lära sig Elixir)
- [Learning Elixir](https://pragprog.com/book/cdc-elixir/learning-elixir) (en bok för att lära sig Elixir från grundläggande till avancerade koncept)