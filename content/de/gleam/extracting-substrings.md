---
title:                "Teilzeichenketten extrahieren"
html_title:           "PowerShell: Teilzeichenketten extrahieren"
simple_title:         "Teilzeichenketten extrahieren"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/gleam/extracting-substrings.md"
---

{{< edit_this_page >}}

## Was & Warum?

Das Extrahieren von Unterstrings ist der Prozess, eine Unterzeichenkette (Substring) von einer Hauptzeichenkette (String) zu gewinnen. Programmierer machen das, um bestimmte Teile eines Textes isoliert zu bearbeiten oder zu analysieren.

## Wie Macht Man Das:

In Gleam könnten wir die `slice` Funktion aus dem `gleam/string` Modul benutzen, um einen Substring zu extrahieren. Hier ist ein einfaches Beispiel:

```Gleam
import gleam/string

fn main() {
  let meinsatz = "Ich lerne Gleam"
  let substring = string.slice(meinsatz, 3, 9)
  io.println(substring) // "lerne"
}
```

Führen Sie dieses Programm aus, und es gibt den Substring "lerne" aus.

## Vertiefung:

Der Umgang mit Strings und Substrings ist ein zentrales Prinzip in der Programmierung und wurde schon in den ältesten Hochsprachen wie FORTRAN implementiert. Es gibt viele Wege, wie Sie das in unterschiedlichen Sprachen machen könnten, und in Gleam, ist es natürlich auch möglich, eigene Funktionen zu schreiben, die Unterstrings extrahieren. Dennoch, die `slice` Funktion ist eine eingebaute und performante Methode zur Unterstring-Extraktion, da sie direkt auf niedrigerer Ebene mit den Bytes des Strings arbeitet.

## Weitere Literatur:
