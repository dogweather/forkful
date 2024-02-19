---
aliases:
- /nl/haskell/using-an-interactive-shell-repl/
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:09:11.542633-07:00
description: "Een interactieve shell, of REPL (Read-Eval-Print Loop), in Haskell laat\
  \ je codefragmenten live uitvoeren. Het is een speeltuin voor snelle feedback, het\u2026"
lastmod: 2024-02-18 23:09:01.900711
model: gpt-4-0125-preview
summary: "Een interactieve shell, of REPL (Read-Eval-Print Loop), in Haskell laat\
  \ je codefragmenten live uitvoeren. Het is een speeltuin voor snelle feedback, het\u2026"
title: Het gebruik van een interactieve shell (REPL)
---

{{< edit_this_page >}}

## Wat & Waarom?
Een interactieve shell, of REPL (Read-Eval-Print Loop), in Haskell laat je codefragmenten live uitvoeren. Het is een speeltuin voor snelle feedback, het testen van functies en het leren van de taal.

## Hoe te:
Om de GHCi (Glasgow Haskell Compiler's interactieve omgeving) te starten, typ je simpelweg `ghci` in je terminal. Hier is hoe je het gebruikt:

```Haskell
Prelude> let x = 5
Prelude> x * 2
10
Prelude> :t x
x :: Num a => a
```

Voorbeelduitvoer legt uit dat `x` een numerieke variabele is en toont dat het verdubbelen ervan resulteert in 10.

## Diepere Duik:
Haskell's GHCi is sinds de introductie een lange weg gekomen. Het biedt een rijke reeks functies zoals tab-aanvulling, invoer over meerdere regels en het laden van pakketten. Alternatieven zoals Hugs zijn nu voornamelijk historisch, met GHCi als de standaard. GHCi compileert code just-in-time elke keer dat je een uitdrukking invoert, waardoor je een efficiënte manier hebt om je Haskell-code te testen.

## Zie Ook:
- [De GHC Gebruikershandleiding – GHCi](https://downloads.haskell.org/ghc/latest/docs/html/users_guide/ghci.html)
- [Learn You a Haskell for Great Good! – Beginnen](http://learnyouahaskell.com/starting-out#hello-world)
- [Haskell Wiki – GHC/GHCi](https://wiki.haskell.org/GHC/GHCi)
