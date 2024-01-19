---
title:                "Eine Zeichenkette interpolieren"
html_title:           "Arduino: Eine Zeichenkette interpolieren"
simple_title:         "Eine Zeichenkette interpolieren"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/haskell/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?

Interpolation von Zeichenketten bezieht sich auf den Prozess der Einbettung von Ausdrücken in Zeichenketten, so dass sie zur Laufzeit ausgewertet werden. Programmierer verwenden es wegen seiner Benutzerfreundlichkeit und Effizienz bei der Erstellung dynamischer Strings.
 
## Anleitung:

In Haskell verwenden wir die Paket `Text.Printf` für Zeichenketteninterpolation. Hier sind einige Beispiele eingebettet.

```Haskell
import Text.Printf

main = do
  let name = "John"
  let age = 29

  let interpolatedString = printf "Hallo %s, du bist %d Jahre alt!\n" name age
  
  putStrLn interpolatedString
```

Wenn Sie das obige Programm ausführen, sehen Sie etwas wie:

```Haskell
Hallo John, du bist 29 Jahre alt!
```
## Vertiefung:

Historisch gesehen, ist Zeichenketteninterpolation ein Konzept, das in der Shell-Programmierung populär wurde und in vielen Hochsprachen übernommen wurde. In Haskell ist die `Text.Printf` Bibliothek für dieses Konzept verantwortlich. Es besteht auch die Alternative, einfache Konkatenation zu verwenden, aber Interpolation ist oft sauberer und lesbarer.

Die Implementierung von `Text.Printf` in Haskell stützt sich auf Typklassen und variadische Funktionen. Sie erzeugt während der Kompilierung spezialisierten Code für jeden interpolierten String.

## Weiterführende Informationen:

Für komplexere Anwendungsfälle und weitere Informationen über Zeichenketteninterpolation in Haskell, kannst du folgende Ressourcen konsultieren:

- Haskell Text.Printf Dokumentation: http://hackage.haskell.org/package/base-4.14.1.0/docs/Text-Printf.html
- Kaufen Sie das Buch "Real World Haskell" hier: http://book.realworldhaskell.org/
- Weitere Informationen zur Zeichenkettenmanipulation in Haskell: https://wiki.haskell.org/Introduction