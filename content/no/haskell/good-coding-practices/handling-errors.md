---
date: 2024-01-26 00:54:03.199861-07:00
description: "H\xE5ndtering av feil i programmering handler om \xE5 styre det uventede\u2014\
  ting som kan g\xE5 galt. Programmerere gj\xF8r dette for \xE5 sikre at deres programmer\
  \ kan\u2026"
lastmod: '2024-03-13T22:44:40.849354-06:00'
model: gpt-4-1106-preview
summary: "H\xE5ndtering av feil i programmering handler om \xE5 styre det uventede\u2014\
  ting som kan g\xE5 galt. Programmerere gj\xF8r dette for \xE5 sikre at deres programmer\
  \ kan\u2026"
title: "Feilh\xE5ndtering"
weight: 16
---

## Hva & Hvorfor?
Håndtering av feil i programmering handler om å styre det uventede—ting som kan gå galt. Programmerere gjør dette for å sikre at deres programmer kan håndtere disse situasjonene med nåde, uten å krasje eller produsere feil resultater.

## Hvordan:
Haskell håndterer feil robust gjennom typer som `Maybe` og `Either`. Her er et raskt blikk:

```Haskell
safeDivide :: Integral a => a -> a -> Maybe a
safeDivide _ 0 = Nothing  -- Det går ikke å dele på null, så vi returnerer Nothing.
safeDivide x y = Just (x `div` y)  -- Ellers er alt bra, returner resultatet i en Just.

-- La oss se det i aksjon:
example1 :: Maybe Int
example1 = safeDivide 10 2  -- Just 5

example2 :: Maybe Int
example2 = safeDivide 10 0  -- Nothing
```

For mer kompleks feilhåndtering, kommer `Either` inn i bildet:

```Haskell
safeDivideEither :: Integral a => a -> a -> Either String a
safeDivideEither _ 0 = Left "Divisjon på null feil."  -- Denne gangen bærer feilen et budskap.
safeDivideEither x y = Right (x `div` y)

-- Og i bruk:
example3 :: Either String Int
example3 = safeDivideEither 10 2  -- Right 5

example4 :: Either String Int
example4 = safeDivideEither 10 0  -- Left "Divisjon på null feil."
```

## Dypdykk
I Haskell-verdenen har feilhåndtering en sterk historie. Tilbake i tiden kunne feil ta ned hele programmet ditt—ikke noe gøy. Haskells typesystem tilbyr måter å gjøre dette mye mindre sannsynlig på. Vi har `Maybe` og `Either`, men det finnes også andre som `Exceptions` og `IO` for forskjellige scenarioer.

`Maybe` er enkel: du får `Just` noe hvis alt er vel, eller `Nothing` hvis det ikke er det. `Either` tar det et skritt videre, som tillater deg å returnere en feilmelding (`Left`) eller et vellykket resultat (`Right`).

Begge er rene, noe som betyr at de ikke roter med den ytre verdenen – en stor sak i Haskell. Vi unngår fallgruvene med ukontrollerte unntak som plager noen andre språk.

For de som ikke er fornøyde med `Maybe` og `Either`, tilbyr biblioteker som `Control.Exception` mer tradisjonell, imperativ-stil feilhåndtering gjennom unntak. Men å bruke dem for liberalt kan komplisere ting, så samfunnet holder seg ofte til typene.

## Se Også
Dykk dypere med:

- Haskells egne dokumenter: [Haskell](https://haskell.org/documentation)
- Flott for nybegynnere: ["Learn You a Haskell for Great Good!"](http://learnyouahaskell.com/)
