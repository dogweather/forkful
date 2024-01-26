---
title:                "Nutzung einer interaktiven Shell (REPL)"
date:                  2024-01-26T04:14:43.343685-07:00
model:                 gpt-4-0125-preview
simple_title:         "Nutzung einer interaktiven Shell (REPL)"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/haskell/using-an-interactive-shell-repl.md"
---

{{< edit_this_page >}}

## Was & Warum?
Eine interaktive Shell oder REPL (Read-Eval-Print Loop) in Haskell ermöglicht es Ihnen, Code-Schnipsel live auszuführen. Es ist ein Spielplatz für schnelles Feedback, das Testen von Funktionen und das Erlernen der Sprache.

## Wie zu:
Um die GHCi (interaktive Umgebung des Glasgow Haskell Compilers) zu starten, tippen Sie einfach `ghci` in Ihre Konsole. So verwenden Sie es:

```Haskell
Prelude> let x = 5
Prelude> x * 2
10
Prelude> :t x
x :: Num a => a
```

Die Beispiel-Ausgabe erklärt, dass `x` eine numerische Variable ist und zeigt, dass sich deren Verdoppelung in 10 ergibt.

## Tiefer eintauchen:
Die GHCi von Haskell hat seit ihrer Einführung einen langen Weg zurückgelegt. Sie bietet einen reichen Satz an Features wie Tab-Vervollständigung, mehrzeilige Eingabe und Paketladung. Alternativen wie Hugs sind mittlerweile größtenteils historisch, wobei GHCi der Standard ist. GHCi kompiliert den Code jedes Mal in Echtzeit, wenn Sie einen Ausdruck eingeben, und bietet Ihnen damit eine effiziente Möglichkeit, Ihren Haskell-Code zu testen.

## Siehe auch:
- [Das GHC-Benutzerhandbuch – GHCi](https://downloads.haskell.org/ghc/latest/docs/html/users_guide/ghci.html)
- [Learn You a Haskell for Great Good! – Einstieg](http://learnyouahaskell.com/starting-out#hello-world)
- [Haskell Wiki – GHC/GHCi](https://wiki.haskell.org/GHC/GHCi)