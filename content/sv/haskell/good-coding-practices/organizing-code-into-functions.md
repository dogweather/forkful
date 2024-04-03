---
date: 2024-01-26 01:10:49.613449-07:00
description: "Hur man g\xF6r: S\xE5 h\xE4r kan du skriva och anv\xE4nda funktioner\
  \ i Haskell."
lastmod: '2024-03-13T22:44:37.960417-06:00'
model: gpt-4-1106-preview
summary: "S\xE5 h\xE4r kan du skriva och anv\xE4nda funktioner i Haskell."
title: Att organisera kod i funktioner
weight: 18
---

## Hur man gör:
Så här kan du skriva och använda funktioner i Haskell:

```Haskell
-- Definierar en enkel funktion för att lägga till två tal
addNumbers :: Int -> Int -> Int
addNumbers x y = x + y

-- Använda funktionen
main = print (addNumbers 3 5)
```

Utskrift:
```
8
```

Du kan också skapa funktioner av högre ordning:

```Haskell
-- Tar en funktion och tillämpar den två gånger på något
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

-- Använder applyTwice med en anonym funktion
main = print (applyTwice (*2) 5)
```

Utskrift:
```
20
```

## Fördjupning
Haskell, ett rent funktionellt språk, behandlar funktioner som förstaklassens medborgare. Historiskt sett är detta rotat i lambda-kalkyl, ett grundläggande ramverk inom datavetenskap. Till skillnad från imperativa språk där funktioner är en sekvens av instruktioner, i Haskell är funktioner uttryck som beskriver relationer mellan data.

Det finns alternativ till att skriva råa funktioner för återanvändning. Överväg att använda typklasser för polymorfism eller att dra nytta av moduler för att gruppera relaterade funktioner. Haskell's lata utvärdering påverkar också funktionernas implementering – funktioner evalueras inte förrän deras resultat behövs, vilket potentiellt kan påverka prestandaöverväganden.

## Se även
- Officiell Haskell-dokumentation: https://www.haskell.org/documentation/
- "Learn You a Haskell for Great Good!" av Miran Lipovača, en nybörjarvänlig bok: http://learnyouahaskell.com/
- "Real World Haskell" av Bryan O'Sullivan, Don Stewart och John Goerzen: http://book.realworldhaskell.org/
