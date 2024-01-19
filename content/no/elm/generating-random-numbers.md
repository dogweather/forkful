---
title:                "Generere tilfeldige tall"
html_title:           "Arduino: Generere tilfeldige tall"
simple_title:         "Generere tilfeldige tall"
programming_language: "Elm"
category:             "Elm"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elm/generating-random-numbers.md"
---

{{< edit_this_page >}}

# Generering av tilfeldige tall med Elm 

### Hva & Hvorfor?
Generering av tilfeldige tall er prosessen med å produsere en sekvens som ikke har noe mønster eller forutsigelse. Programmere bruker dette til spill, simuleringer, kryptografi, og for å skape unike ID-er.

### Hvordan:
Her er den grunnleggende måten å generere et tilfeldig tall i Elm.

```Elm
import Random exposing (Generator, int, random)

randomInt : Int -> Int -> Generator Int
randomInt min max = int min max

main = 
  random generate (randomInt 1 100)
```
Koden ovenfor vil gi en vilkårlig verdi mellom 1 og 100.

### Dyp Dykk 
Elm bruker pseudotilfeldige tallgeneratorer for å generere tilfeldige tall. Historisk sett er dette en vanlig praksis i programmering, og den sikrer en høy grad av tilfeldighet og uforutsigbarhet.

Alternativ til elm sin innebygde random-modul er elms egen random-extra-pakke. Den gir mer funksjonalitet, for eksempel generering av tilfeldige flerpunktsnummere.

På implementasjonsnivå bruker elm en funksjonellt programmeringsparadigm for å generere tilfeldige tall. Dette betyr at genereringen er ren, hvilket betyr at for de samme inngangverdiene vil du alltid få samme tilfeldige tall. 

### Se Også: 
Random modul dokumentasjon: https://package.elm-lang.org/packages/elm/random/latest/
Extra-random pakken: https://package.elm-lang.org/packages/elm/random-extra/latest/