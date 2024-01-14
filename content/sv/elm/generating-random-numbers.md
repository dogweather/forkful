---
title:                "Elm: Generering av slumpmässiga nummer"
simple_title:         "Generering av slumpmässiga nummer"
programming_language: "Elm"
category:             "Elm"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elm/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Varför

Att generera slumpmässiga nummer i Elm kan vara användbart för olika ändamål, som att skapa slumpmässiga spel eller testa olika algoritmer. Det kan också vara ett bra sätt att utöka ens kunskap om Elm-programmering.

## Hur man genererar slumpmässiga nummer i Elm

Vi kan använda funktionen `Random.generate` tillsammans med `Random.int` för att generera ett slumpmässigt heltal inom en given gräns. Se nedan för ett exempel på hur detta kan se ut i Elm:

```Elm
import Random

Random.generate randomNumber (Random.int 1 10)

randomNumber num =
    Debug.log "Slumpmässigt nummer:" num
```
Detta kodblock kommer att generera ett slumpmässigt nummer mellan 1 och 10 och skriva ut det i konsolen. Om du kör koden högre upp under en kort period kommer du att se olika slumpmässiga nummer visas varje gång. Det är det som är så kul med slumpmässiga nummer!

För att använda `Random.generate` för att generera ett slumpmässigt nummer av en annan typ, som en flyttal eller en lista, kan du använda `Random.float` respektive `Random.list`. Det finns också andra funktioner i Random-modulen som du kan utforska för att skapa ännu mer avancerad slumpmässighet.

## Djupdykning i slumpmässiga nummer

Bakom kulisserna använder `Random.generate` faktiskt följande typ av funktion:

```Elm
Random.generate : (a -> msg) -> Generator a -> Cmd msg
```

Funktionen tar ett argument som specificerar vilken typ av värden som ska genereras, en generator (en typ som används för att producera slumpmässiga värden) och returnerar en `Cmd msg`, som är en Elm-kommando som kan utföras för att generera ett slumpmässigt nummer. Denna process kallas för "lättelsen" ("lifting" på engelska) av genereringen av värden.

Det finns även andra funktioner i Random-modulen som kan användas tillsammans med `Random.generate`, till exempel `Random.generateList`, som genererar en lista av slumpmässiga värden baserat på en given generator.

## Se också

- [Elm Random-biblioteket](https://package.elm-lang.org/packages/elm/random/latest/)
- [Elm Random-exempel](https://github.com/elm/random/tree/master/examples)
- [Officiell Elm-dokumentation om Random](https://guide.elm-lang.org/effects/random.html)