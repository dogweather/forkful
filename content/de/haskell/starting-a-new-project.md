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

---

## Warum

Warum sollte man sich überhaupt die Mühe machen, ein neues Projekt zu starten? Ganz einfach: Weil es Spaß macht! Außerdem ermöglicht es dir, neue Fähigkeiten zu erlernen und dein Wissen zu erweitern.

## So geht's

Um ein neues Haskell-Projekt zu starten, gibt es einige Schritte zu beachten. Zunächst musst du sicherstellen, dass du Haskell auf deinem Computer installiert hast. Anschließend kannst du mit dem Befehl `stack new mein_projekt` in der Kommandozeile dein neues Projekt erstellen. In der generierten Datei `mein_projekt.cabal` kannst du die Projekteinstellungen wie Name, Version und Autor anpassen. Dann kannst du mit dem Befehl `stack build` dein Projekt bauen und mit `stack exec mein_projekt` ausführen. 

```Haskell
main :: IO ()
main = putStrLn "Hallo Welt!"
```

```
Ausgabe: Hallo Welt!
```

## Tiefergehende Einblicke

Bevor du mit der Entwicklung deines Projekts loslegst, solltest du dir Gedanken über die Struktur machen und welche Funktionen du implementieren möchtest. Eine gute Möglichkeit ist es, sich an vorhandenen Haskell-Projekten zu orientieren und ihre Struktur zu studieren. Auch das Schreiben von Tests und das Verwenden von Bibliotheken können dir dabei helfen, effizienter zu programmieren. Vergiss außerdem nicht, regelmäßig zu reflektieren und deinen Code zu verbessern.

## Siehe auch

- [Offizielle Haskell-Website](https://www.haskell.org/)
- [Stack-Dokumentation](https://docs.haskellstack.org/en/stable/README/)
- [Haskell für Anfänger: Eine Einführung in die funktionale Programmierung](https://www.youtube.com/watch?v=CNrCvfaaL-o)