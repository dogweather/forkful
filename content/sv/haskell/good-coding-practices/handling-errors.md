---
date: 2024-01-26 00:53:15.582051-07:00
description: "Att hantera fel i programmering handlar om att f\xF6rvalta det ov\xE4\
  ntade\u2014saker som kan g\xE5 fel. Programmerare g\xF6r detta f\xF6r att s\xE4\
  kerst\xE4lla att deras program\u2026"
lastmod: '2024-03-13T22:44:37.962251-06:00'
model: gpt-4-1106-preview
summary: "Att hantera fel i programmering handlar om att f\xF6rvalta det ov\xE4ntade\u2014\
  saker som kan g\xE5 fel."
title: Hantering av fel
weight: 16
---

## Hur man gör:
Haskell hanterar fel robust genom typer som `Maybe` och `Either`. Här är en snabbtitt:

```Haskell
safeDivide :: Integral a => a -> a -> Maybe a
safeDivide _ 0 = Nothing  -- Att dela med noll går inte, så vi returnerar Nothing.
safeDivide x y = Just (x `div` y)  -- Annars är allt bra, returnera resultatet i en Just.

-- Låt oss se det i aktion:
example1 :: Maybe Int
example1 = safeDivide 10 2  -- Just 5

example2 :: Maybe Int
example2 = safeDivide 10 0  -- Nothing
```

För mer komplex felsökning träder `Either` i spel:

```Haskell
safeDivideEither :: Integral a => a -> a -> Either String a
safeDivideEither _ 0 = Left "Divide by zero error."  -- Den här gången medföljer ett felmeddelande.
safeDivideEither x y = Right (x `div` y)

-- Och i användning:
example3 :: Either String Int
example3 = safeDivideEither 10 2  -- Right 5

example4 :: Either String Int
example4 = safeDivideEither 10 0  -- Left "Divide by zero error."
```

## Djupdykning
I Haskell-världen har felsökning en lång historia. Förr kunde fel få ett helt program att rasa—inte kul. Haskells typsystem erbjuder sätt att göra detta mycket mindre sannolikt. Vi har `Maybe` och `Either`, men det finns andra som `Exceptions` och `IO` för olika scenarier.

`Maybe` är enkelt: du får `Just` någonting om allt är väl, eller `Nothing` om det inte är det. `Either` tar det ett steg vidare, vilket tillåter dig att returnera ett felmeddelande (`Left`) eller ett framgångsrikt resultat (`Right`).

Båda är rena, vilket innebär att de inte stör den yttre världen – en stor sak i Haskell. Vi undviker fallgroparna med okontrollerade undantag som plågar vissa andra språk.

För de som inte är nöjda med `Maybe` och `Either`, erbjuder bibliotek som `Control.Exception` mer traditionell, imperativ-stil felhantering genom undantag. Men att använda dem för liberalt kan komplicera saker, så ofta håller sig samhället till typerna.

## Se även
Fördjupa dig med:

- Haskells egna dokument: [Haskell](https://haskell.org/documentation)
- Bra för nybörjare: ["Learn You a Haskell for Great Good!"](http://learnyouahaskell.com/)
