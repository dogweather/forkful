---
date: 2024-01-26 03:49:21.766038-07:00
description: "Ein Debugger wird verwendet, um tief in Ihren Code einzutauchen, mit\
  \ Werkzeugen, die darauf ausgelegt sind, ein Programm w\xE4hrend der Ausf\xFChrung\
  \ zu\u2026"
lastmod: '2024-03-11T00:14:27.833029-06:00'
model: gpt-4-0125-preview
summary: "Ein Debugger wird verwendet, um tief in Ihren Code einzutauchen, mit Werkzeugen,\
  \ die darauf ausgelegt sind, ein Programm w\xE4hrend der Ausf\xFChrung zu\u2026"
title: Einsatz eines Debuggers
---

{{< edit_this_page >}}

## Was & Warum?
Ein Debugger wird verwendet, um tief in Ihren Code einzutauchen, mit Werkzeugen, die darauf ausgelegt sind, ein Programm während der Ausführung zu inspizieren, anzuhalten und zu manipulieren. Programmierer tun dies, um Fehlern nachzujagen, den Programmablauf zu verstehen und sicherzustellen, dass ihr Code genau das tut, was sie erwarten.

## Wie man es macht:
Lassen Sie uns einen Spaziergang mit GHCi machen, Haskeals interaktiver Umgebung, die als einfacher Debugger fungieren kann. Sie starten es mit Ihrem Haskell-Code und beginnen herumzustochern. Hier ist ein Beispiel:

```Haskell
main :: IO ()
main = do
    putStrLn "Hey, wie heißt du?"
    name <- getLine
    putStrLn $ "Hallo, " ++ name ++ "! Lass uns debuggen."
    let result = buggyFunction 5
    print result

buggyFunction :: Int -> Int
buggyFunction n = n * 2 -- Tun Sie so, als wäre hier ein Fehler
```

Um mit GHCi das Debugging zu starten:

```bash
$ ghci IhreHaskellDatei.hs
```

Setzen Sie einen Breakpoint bei `buggyFunction`:

```Haskell
Prelude> :break buggyFunction
```

Führen Sie Ihr Programm aus:

```Haskell
Prelude> :main
Hey, wie heißt du?
```

Ihr Programm hält bei `buggyFunction` an. Jetzt können Sie Variablen inspizieren, durch den Code schrittweise gehen und Ausdrücke auswerten.

## Tiefere Einblicke:
Historisch bedingt führte Haskells Ruf für reine Funktionen und starke Typisierung zu der Annahme, dass Debugging-Werkzeuge weniger kritisch seien. Die Realität sieht anders aus – komplexe Programme profitieren immer von guten Debugging-Werkzeugen. GHCi bietet grundlegende Debugging-Befehle. Für ein visuelleres Erlebnis oder größere Anwendungen könnten Sie jedoch IDEs mit integrierten Debuggern erkunden, wie Visual Studio Code mit Haskell-Erweiterungen oder das Haskell-Plugin von IntelliJ.

Alternativen zum Debugger umfassen die Verwendung von Print-Anweisungen, bekannt als „printf debugging“, oder das Ausnutzen von Haskells starkem Typsystem, um falsche Zustände unrepräsentierbar zu machen. Doch manchmal ersetzt nichts das Schritt-für-Schritt-Durchgehen des Codes.

Was die Implementierungsdetails angeht, so arbeitet der Debugger von Haskell mit dem Laufzeitsystem zusammen. Er kann Breakpoints handhaben, die Ausführung schrittweise durchführen und die Inspektion von Variablen ermöglichen. Da Haskell jedoch faul ausgewertet wird, können die Dinge etwas unintuitiv werden. Das Debuggen eines Haskell-Programms bedeutet oft, ein Auge darauf zu haben, wann und wie Ausdrücke ausgewertet werden.

## Siehe auch:
- [GHC Benutzerhandbuch - Debugger](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/debugging.html)
- [IntelliJ Haskell Plugin](https://plugins.jetbrains.com/plugin/8258-intellij-haskell)
