---
title:                "Generera slumpmässiga nummer"
html_title:           "Arduino: Generera slumpmässiga nummer"
simple_title:         "Generera slumpmässiga nummer"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elixir/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Generera slumpmässiga nummer innebär att skapa nummer som inte är förutsägbara eller har någon ordning. Programmerare gör detta när de behöver unika data, testar robustheten hos program eller skapar slumpmässiga händelser i spel.

## Hur man gör:

För att generera slumpmässiga nummer i Elixir kan du använda `:rand.uniform` funktionen i Erlang. Här är ett enkelt exempel:

```Elixir
IO.puts(:rand.uniform()) # generar ett flyttal mellan 0 och 1
```

För att åstadkomma ett nummer inom ett visst intervall, ange intervallet som en parameter till funktionen:

```Elixir
IO.puts(:rand.uniform(100)) # generar ett heltal mellan 1 och 100
```

## Djupdykning

Slumpmässiga nummer har en lång historia i datavetenskap och tillämpas i en mängd olika sammanhang, från kryptografi till Monte Carlo-metoden. Erlangs `:rand`-modul, som Elixir bygger på, ger en snabb och effektiv generator för pseudo-slumpmässiga nummer.

Alternativa metoder för att generera slumpmässiga nummer inkluderar användning av externa tjänster som levererar verkligt slumpmässiga tal, eller algoritmer som genererar kryptografiskt säkra slumpmässiga tal. 

Implementeringen av `:rand.uniform` i Elixir är densamma som i Erlang. Det är en pseudoslumpmässig generator som använder en Mersenne Twister-algoritm, vilket ger en jämn fördelning och snabb hastighet.

## Se även:

- Erlangs officiella dokumentation om :rand-modulen: [http://erlang.org/doc/man/rand.html](http://erlang.org/doc/man/rand.html)
- Elixir officiella dokumentation: [https://elixir-lang.org/docs.html](https://elixir-lang.org/docs.html)
- Artikel om användning av slumpmässiga nummer i programmering: [https://medium.com/@FrontMage/use-random-numbers-in-programming-87663ffeb3a4](https://medium.com/@FrontMage/use-random-numbers-in-programming-87663ffeb3a4)