---
title:                "Elm: Generering av slumpmässiga nummer"
programming_language: "Elm"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elm/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Varför

Att generera slumpmässiga nummer är en viktig del av många program, från spel till dataanalys. Med Elm kan du enkelt skapa en tillförlitlig och slumpmässig algoritm för att skapa dessa nummer.

## Hur Man Gör

Först importera "Random" paketet i din fil och sedan använda funktionen "generate" för att skapa ett slumpmässigt nummer. Här är ett exempel på hur du kan skapa ett slumpmässigt heltal mellan 1 och 10:

```
Elm.Random.generate randomInt 1 10

randomInt: Int
```

För att få ett annat slumpmässigt nummer, kan du skapa en ny funktion och använda den för att generera ett annat tal. Till exempel:

```
generateRandomNumber: Int
generateRandomNumber =
    Elm.Random.generate anotherRandomInt 1 100

anotherRandomInt: Int
```

## Djupdykning

När du skapar ett slumpmässigt tal, genererar Elm det baserat på en seed (frö) som används för att skapa en pseudo-slumpmässig sekvens. Det är viktigt att förstå att fröet måste vara unikt för att få olika slumpmässiga nummer. Om du använder samma frö flera gånger, kommer du att få samma sekvens av slumpmässiga nummer.

Du kan också använda funktionen "step" för att skapa ett steg i din sekvens av slumpmässiga nummer. Detta kan vara användbart för spel eller simuleringar där du vill ha en förutsägbar sekvens av slumpmässiga händelser.

## Se Också

- [Elm Documentation for Random](https://package.elm-lang.org/packages/elm/random/latest/)
- [MDN Web Docs - Math.random()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/random)
- [Wikipedia - Pseudorandom number generator](https://en.wikipedia.org/wiki/Pseudorandom_number_generator)