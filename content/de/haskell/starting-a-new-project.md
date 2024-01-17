---
title:                "Ein neues Projekt beginnen"
html_title:           "Haskell: Ein neues Projekt beginnen"
simple_title:         "Ein neues Projekt beginnen"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/haskell/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Was & Warum?

Wenn du ein neues Programmierprojekt startest, beginnst du damit, eine neue Softwareanwendung oder Bibliothek zu erstellen. Programmierer machen das, um neue Ideen zu verwirklichen, Probleme zu lösen oder einfach um etwas Neues zu lernen.

## Wie geht's:

```Haskell
main = putStrLn "Hello World!"

main2 = do
  putStrLn "What's your name?"
  name <- getLine
  putStrLn ("Nice to meet you, " ++ name ++ "!")
```

Output:

```
Hello World!
What's your name?
John
Nice to meet you, John!
```

## Tief tauchen:

Das Konzept, ein neues Projekt zu starten, ist eng mit der Entwicklung von Software und Technologie verbunden. In der Geschichte der Programmierung haben sich verschiedene Methoden und Werkzeuge für das Starten von Projekten entwickelt, darunter agile Methoden und DevOps-Praktiken. Alternativen zum Starten von Projekten können auch die Nutzung von vorhandenen Open-Source-Bibliotheken oder das Einbinden von APIs in deine Anwendung umfassen. Bei der Implementierung eines neuen Projekts ist es wichtig, ein gutes Verständnis der gewählten Programmiersprache und der zugrunde liegenden Konzepte zu haben.

## Siehe auch:

- [Haskell.org](https://www.haskell.org/)
- [Haskell Wikibook](https://en.wikibooks.org/wiki/Haskell)
- [Haskell Reddit](https://www.reddit.com/r/haskell/)