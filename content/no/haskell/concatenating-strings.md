---
title:                "Haskell: Sammenslåing av strenger"
simple_title:         "Sammenslåing av strenger"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/haskell/concatenating-strings.md"
---

{{< edit_this_page >}}

## Hvorfor

Å koble sammen strenger, også kjent som "concatenation", er en vanlig oppgave i programmering. Dette er spesielt nyttig når vi ønsker å slå sammen tekststrenger for å lage en lengre uttrykk. I Haskell, er dette enkelt å gjøre og kan hjelpe til å gjøre koden mer lesbar.

## Hvordan

I Haskell, bruker vi operatoren "++" for å koble sammen to strenger. La oss si at vi ønsker å kombinere to navn, "Per" og "Olsen", for å danne navnet "Per Olsen". Dette kan gjøres ved å skrive følgende kode:

```Haskell
navn1 = "Per"
navn2 = "Olsen"
navn = navn1 ++ " " ++ navn2

print navn
```

Output:

```
Per Olsen
```

Vi kan også kombinere flere strenger på en gang. La oss si at vi ønsker å danne navnet "Mary Anne Johnson". Dette kan gjøres ved å skrive:

```Haskell
navn1 = "Mary"
navn2 = "Anne"
etternavn = "Johnson"
navn = navn1 ++ " " ++ navn2 ++ " " ++ etternavn

print navn
```

Output:

```
Mary Anne Johnson
```

Det er viktig å merke seg at operatoren "++" også kan brukes på andre typer data, ikke bare strenger. Vi kan for eksempel kombinere to lister:

```Haskell
liste1 = [1,2,3]
liste2 = [4,5,6]
liste = liste1 ++ liste2

print liste
```

Output:

```
[1,2,3,4,5,6]
```

## Dypdykk

I Haskell, er funksjonen "++" faktisk en del av "Monoid" klassen. Dette betyr at det finnes visse egenskaper og regler som gjelder for denne funksjonen. For eksempel, er "++" assosiativ, det betyr at rekkefølgen av koble sammen strenger ikke spiller noen rolle. Dette betyr at "a ++ (b ++ c)" er det samme som "(a ++ b) ++ c".

En annen egenskap er at det finnes en "nøytral" verdi, som ikke endrer resultatet når den brukes med "++". I tilfelle av strenger, er den nøytrale verdien en tom streng, "". Dette betyr at "" ++ "Hei" er det samme som "Hei". Dette kan være nyttig å vite når man jobber med mer komplekse kodesnutter.

## Se også

* [Haskell Documentation](https://www.haskell.org/documentation/)
* [Learn You a Haskell](http://learnyouahaskell.com/chapters)
* [Real World Haskell](http://book.realworldhaskell.org/read)