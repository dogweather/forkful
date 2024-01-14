---
title:    "Elm: Generera slumpmässiga nummer"
keywords: ["Elm"]
---

{{< edit_this_page >}}

## Varför

Att generera slumpmässiga nummer är en viktig del av många programmeringsprojekt. Det kan användas för att skapa spel, simuleringar och andra interaktiva applikationer. Elm erbjuder en enkel och effektiv metod för att skapa slumpmässiga nummer, vilket gör det till ett populärt val för många utvecklare.

## Hur man gör det

För att generera slumpmässiga nummer i Elm, använder man sig av funktionen `Random.generate`. Denna funktion tar in två argument - en generator och en önskad typ av data. Generatorn bestämmer vilken typ av slumpmässigt nummer som ska genereras och vilket intervall det ska vara inom. Här är ett exempel på hur man kan använda denna funktion för att generera ett slumpmässigt heltal mellan 1-10:

```Elm
import Random

randomNumber : Int
randomNumber =
    Random.generate (Random.int 1 10)
```

Om vi nu vill generera ett slumpmässigt decimaltal mellan 0 och 1, skulle vi använda en annan typ av generator:

```Elm
import Random

randomDecimal : Float
randomDecimal =
    Random.generate (Random.float 0 1)
```

Som du kan se är det väldigt enkelt att använda `Random.generate` för att skapa slumpmässiga nummer i Elm.

## Djupdykning

För de som vill ha mer kontroll över hur slumpmässiga nummer genereras kan man använda sig av `Random.generateWithSeed`. Denna funktion tar in ett extra argument - en seed eller ett startvärde. Genom att ändra på seedet kan man få olika slutsatser från samma generator. Detta kan vara användbart för att skapa till synes slumpmässiga resultat som ändå är förutbestämda.

## Se även

- [Officiell Elm documentation för Random](https://package.elm-lang.org/packages/elm/random/latest/)
- [24 Days of Elm - Day 13: Generating Random Numbers](https://elm.christmas/2018/13)
- [Elm in Action - Chapter 5: Randomness and Simulations](https://www.manning.com/books/elm-in-action?a_bid=f86b0cf1&a_aid=elm&action=write-review)