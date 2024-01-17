---
title:                "Utskrift av felsökningsutdata"
html_title:           "Ruby: Utskrift av felsökningsutdata"
simple_title:         "Utskrift av felsökningsutdata"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/ruby/printing-debug-output.md"
---

{{< edit_this_page >}}

### Vad & Varför?
Printning av debug-utmatning är när en programmerare inkluderar kod för att skriva ut information om vad som händer i ett program, vanligtvis för att lösa problem eller förstå koden bättre.

## Hur man:
För att printa debug-utmatning i Ruby, använd "p" eller "puts" kommandot följt av den variabel eller kod du vill skriva ut, inom ```Ruby ... ``` kodblock. Här är ett exempel på hur man printar en variabel "name":

```
name = "Ruby"
p name
```

Detta kommer att ge utmatning "Ruby" i terminalen. Det kan också vara användbart att printa olika steg i en loop eller olika värden i en array för att följa hur koden fungerar.

## Djupdykning:
Printning av debug-utmatning är ett vanligt felsökningsverktyg för programmerare, särskilt när det kommer till att förstå koden bättre och hitta problem. Innan datorer var vanliga, använde programmerare ofta utskrifter på papper för att se vad som låg i variabler eller vad som hände vid en viss punkt i koden. Numera har vi terminalen för att göra detta enklare och mer effektivt.

En annan metod för att printa debug-utmatning är att använda ett debugging verktyg, som till exempel Pry. Detta kan vara mer användbart för mer komplexa problem eller när man behöver debugga i realtid.

## Se också:
Om du vill lära dig mer om printning av debug-utmatning i Ruby, kolla in följande resurser:

- [Ruby debugging tutorial](https://www.rubyguides.com/2019/04/ruby-debugging/)
- [Pry debugging tool](https://rubygems.org/gems/pry)
- [Debugging with print statements](https://ruby-doc.org/core-3.0.1/Kernel.html#method-i-p) (officiell dokumentation)