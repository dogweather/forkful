---
title:    "Gleam: Skriva tester"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/gleam/writing-tests.md"
---

{{< edit_this_page >}}

## Varför

Att skriva tester är en viktig del av programmering, eftersom det hjälper dig att säkerställa att din kod fungerar som den ska och uppfyller de önskade resultat. Det är också ett bra sätt att upptäcka buggar och felaktigheter i koden, vilket sparar dig tid och frustration på lång sikt.

## Hur man gör det

Att skriva tester i Gleam är enkelt och effektivt. Det första steget är att skapa en testmapp där du kan placera alla dina testfiler. Detta kan du göra genom att använda kommandot "mix new tests" i terminalen.

Nästa steg är att skapa en testfil för varje del av din kod du vill testa. I dessa filer kan du använda Gleams inbyggda testbibliotek för att skriva dina tester. Ett exempel på detta kan se ut så här:

```
Gleam-tester

  testar "Summera två tal" {
    låt f = funktion (x, y) {
      x + y
    }

    låt resultat = f(2, 3)
    förväntat(resultat) == 5
  }
```

I detta exempel skapar vi en funktion som summerar två tal och testar sedan att resultatet blir rätt. Om testet passerar kommer du att se ett grönt meddelande i terminalen, annars visas ett rött meddelande som indikerar att något gick fel.

Du kan också använda Gleams inbyggda mockningsfunktioner för att testa komplexa funktioner. Detta gör det möjligt att simulera data och testa olika scenarier utan att behöva skriva långdragna kodbaser.

## En djupdykning

Att skriva tester kan vara en djupdykning i din kod eftersom det tvingar dig att tänka på alla olika fall och potentiella problem som din kod kan stöta på. Det hjälper också till att förbättra din kod och göra den mer robust och lätt att underhålla.

Ett annat fördel med att skriva tester är att det gör det enklare att arbeta i team eftersom alla kan förstå och testa koden på samma sätt. Detta minskar risken för konflikter och felaktigheter vid samarbete.

## Se också

- [Gleam-tester dokumentation](https://gleam.run/testing)
- [Gleam-tester exempelkod](https://github.com/gleam-lang/gleam/tree/main/examples/testing)
- [Ett djupare dyk in i testning](https://www.chriskipp.com/blog/2018/02/test-driven-development-tutorial/)