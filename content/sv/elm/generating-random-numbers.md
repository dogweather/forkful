---
title:    "Elm: Generera slumpmässiga nummer"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/elm/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Varför

Att generera slumpmässiga siffror är en viktig del av många programmeringsprojekt, oavsett om det handlar om spel, matematiska simuleringar eller slumpvist val av data. Elm har ett inbyggt funktion för att generera slumpmässiga nummer, vilket gör det enkelt att implementera i dina projekt.

## Hur man gör

För att generera slumpmässiga nummer i Elm använder man funktionen `Random.generate`. Den tar emot två argument: en genererare och en signal som ska trigga genereringen av ett nytt slumpmässigt nummer.

```Elm
Random.generate randomNumberGenerator NewRandomNumber
```

För att skapa en genererare använder man funktionen `Random.int` som tar emot två parametrar: startvärdet och slutvärdet för det slumpmässiga numret som ska genereras.

```Elm
randomNumberGenerator : Random.Generator Int
randomNumberGenerator =
  Random.int 1 10
```

När signalen `NewRandomNumber` triggas kommer ett nytt slumpmässigt nummer att genereras. Detta nummer kan sedan användas i ditt projekt på olika sätt.

```Elm
NewRandomNumber : (Result Error Int -> msg) -> Sub msg
```

## Djupdyk

För mer komplexa fall, som att generera en bokstav eller en sanningsvärde, finns det andra funktioner som `Random.chance` eller `Random.bool` tillgängliga. Man kan också kombinera flera genererare för att skapa mer avancerade mönster, till exempel med hjälp av funktionen `Random.generate2`.

Det är också möjligt att skapa en återanvändbar genererare genom att använda funktionen `Random.map` som gör det möjligt att modifiera ett slumpmässigt nummer efter det har genererats.

## Se även

- Officiell dokumentation för "Random"-paketet i Elm: <https://package.elm-lang.org/packages/elm/random/latest/>
- Tutorial för att generera slumpmässiga tal i Elm: <https://thoughtbot.com/blog/random-numbers-in-elm>
- Exempelprojekt som använder generering av slumpmässiga tal i Elm: <https://github.com/ckirknielsen/elm-random-example>