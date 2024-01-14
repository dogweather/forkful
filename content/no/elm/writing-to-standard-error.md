---
title:                "Elm: Skrive til standardfeil"
programming_language: "Elm"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elm/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Hvorfor

Å skrive til standard error er en viktig del av å utvikle programmer på Elm-plattformen. Det lar deg håndtere feil og unntak på en effektiv måte, og sørger for at du kan finne og fikse problemer raskt.

## Slik gjør du det

For å skrive til standard error i din Elm-kode, kan du benytte deg av funksjonen `Debug.log`. Den tar to argumenter - en streng som inneholder en beskrivelse av det du vil skrive til error, og deretter verdien du ønsker å logge.

```
Elm.debug("Feil på linje 25", 42)
```

Dette vil skrive følgende til standard error:

```
Feil på linje 25: 42
```

Du kan også skrive til standard error direkte ved hjelp av funksjonen `stderr`, som tar et enkelt argument:

```
stderr("En feil har oppstått")
```

Dette vil skrive følgende til standard error:

```
En feil har oppstått
```

## Dypdykk

Når du skriver til standard error, bør du tenke på å kun logge nødvendig informasjon. For mye logging kan gjøre det vanskelig å finne og fikse feil. Det kan også påvirke ytelsen til applikasjonen din.

Følg også med på eventuelle advarsler som kommer opp under testing og debugging av koden din. Disse kan indikere potensielle problemer og bør løses så snart som mulig.

## Se også

- [Elm Error Messages](https://elm-lang.org/docs/error-messages)
- [Elm - Debug Module](https://package.elm-lang.org/packages/elm/core/latest/Debug)
- [Elm - stderr function](https://package.elm-lang.org/packages/elm/core/latest/Basics#stderr)