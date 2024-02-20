---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:01:57.797759-07:00
description: "Foutenafhandeling in programmeren gaat over het beheren van het onverwachte\u2014\
  zaken die mis kunnen gaan. Programmeurs doen dit om ervoor te zorgen dat hun\u2026"
lastmod: 2024-02-19 22:05:09.930589
model: gpt-4-0125-preview
summary: "Foutenafhandeling in programmeren gaat over het beheren van het onverwachte\u2014\
  zaken die mis kunnen gaan. Programmeurs doen dit om ervoor te zorgen dat hun\u2026"
title: Fouten afhandelen
---

{{< edit_this_page >}}

## Wat & Waarom?
Foutenafhandeling in programmeren gaat over het beheren van het onverwachte—zaken die mis kunnen gaan. Programmeurs doen dit om ervoor te zorgen dat hun programma’s deze situaties op een elegante manier kunnen afhandelen, zonder te crashen of verkeerde resultaten te produceren.

## Hoe:
Haskell handelt fouten robuust af door middel van typen zoals `Maybe` en `Either`. Hier is een snelle blik:

```Haskell
safeDivide :: Integral a => a -> a -> Maybe a
safeDivide _ 0 = Nothing  -- Delen door nul gaat niet, dus we geven Nothing terug.
safeDivide x y = Just (x `div` y)  -- Anders, alles goed, we geven het resultaat terug in een Just.

-- Laten we dit in actie zien:
example1 :: Maybe Int
example1 = safeDivide 10 2  -- Just 5

example2 :: Maybe Int
example2 = safeDivide 10 0  -- Nothing
```

Voor complexere foutafhandeling, speelt `Either` een rol:

```Haskell
safeDivideEither :: Integral a => a -> a -> Either String a
safeDivideEither _ 0 = Left "Fout bij delen door nul."  -- Deze keer draagt de fout een bericht.
safeDivideEither x y = Right (x `div` y)

-- En in gebruik:
example3 :: Either String Int
example3 = safeDivideEither 10 2  -- Right 5

example4 :: Either String Int
example4 = safeDivideEither 10 0  -- Left "Fout bij delen door nul."
```

## Diepere Duik
In de Haskell-wereld heeft foutafhandeling een sterke historie. Vroeger konden fouten je hele programma neerhalen—niet leuk. Haskell's typesysteem biedt manieren om dit veel minder waarschijnlijk te maken. We hebben `Maybe` en `Either`, maar er zijn andere zoals `Exceptions` en `IO` voor verschillende scenario's.

`Maybe` is eenvoudig: je krijgt `Just` iets als alles goed gaat, of `Nothing` als dat niet zo is. `Either` gaat een stap verder, waardoor je een foutmelding (`Left`) of een succesvol resultaat (`Right`) kunt teruggeven.

Beide zijn puur, wat betekent dat ze niet interageren met de buitenwereld – een groot punt in Haskell. We vermijden de valkuilen van ongecontroleerde uitzonderingen die sommige andere talen plagen.

Voor degenen die niet tevreden zijn met `Maybe` en `Either`, bieden bibliotheken zoals `Control.Exception` meer traditionele, imperatieve stijl foutafhandeling door middel van uitzonderingen. Maar ze te vrij gebruiken kan dingen compliceren, dus de gemeenschap houdt vaak vast aan de typen.

## Zie Ook
Duik dieper met:

- Haskell's eigen documentatie: [Haskell](https://haskell.org/documentation)
- Geweldig voor beginners: ["Learn You a Haskell for Great Good!"](http://learnyouahaskell.com/)
