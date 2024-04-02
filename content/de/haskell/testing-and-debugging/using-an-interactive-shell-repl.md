---
date: 2024-01-26 04:14:43.343685-07:00
description: "Eine interaktive Shell oder REPL (Read-Eval-Print Loop) in Haskell erm\xF6\
  glicht es Ihnen, Code-Schnipsel live auszuf\xFChren. Es ist ein Spielplatz f\xFC\
  r\u2026"
lastmod: '2024-03-13T22:44:53.933086-06:00'
model: gpt-4-0125-preview
summary: "Eine interaktive Shell oder REPL (Read-Eval-Print Loop) in Haskell erm\xF6\
  glicht es Ihnen, Code-Schnipsel live auszuf\xFChren. Es ist ein Spielplatz f\xFC\
  r\u2026"
title: Nutzung einer interaktiven Shell (REPL)
weight: 34
---

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
