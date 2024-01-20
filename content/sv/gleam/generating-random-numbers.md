---
title:                "Generera slumpmässiga nummer"
html_title:           "Arduino: Generera slumpmässiga nummer"
simple_title:         "Generera slumpmässiga nummer"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/gleam/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att generera slumpmässiga nummer betyder att skapa nummer utan någon märkbar mönster eller upprepning. Programmerare gör detta för att simulera slumpmässighet och osäkerhet i program och algoritmer.

## Hur man Gör:

Gleam har inbyggt stöd för att generera slumpmässiga nummer. Följande exempel demonstrerar hur du kan generera ett slumpmässigt heltal mellan 1 och 10.

``` Gleam
import gleam/random.{Generator}

fn main() {
  let generator = Generator.default()
  let (number, next) = generator.int(1, 10)
  assert Ok(_) = io.println(number)
}
```

Kör programmet och se output:

```
> gleam run .
7
```

Notera att varje körning av programmet kommer att producera ett annat tal.

## Djup Dykning

Historiskt sett har programmerare alltid behövt generera slumpmässiga nummer för olika uppgifter, från simulering av komplexa system till skapande av unika identifierare. Det finns alternativ till Gleams metoder, som att använda operativsystemets inbyggda funktioner eller att skriva en egen slumpgenerator från grunden. Det är dock enklast att använda ett inbyggt bibliotek som Gleam tillhandahåller. Generatorn använder en version av Mersenne Twister algoritmen för att skapa dess numeriska sekvens.

## Se Även

- Gleam Random modul dokumentation: https://hexdocs.pm/gleam_stdlib/gleam/random 
- Mersenne Twister: https://en.wikipedia.org/wiki/Mersenne_Twister
- Introduktion till Random Number Generation: https://towardsdatascience.com/introduction-to-random-number-generators-98dc7e0d842d