---
title:                "Einen String in Kleinbuchstaben umwandeln"
html_title:           "Elm: Einen String in Kleinbuchstaben umwandeln"
simple_title:         "Einen String in Kleinbuchstaben umwandeln"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/gleam/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# Zeichenkette in Kleinbuchstaben umwandeln in Gleam

## Was und Warum?

Das Umwandeln einer Zeichenkette (oder eines "Strings") in Kleinbuchstaben ist eine gängige Aktion beim Arbeiten mit Text in der Programmierung. Entwickler tun dies, um Textunempfindlichkeit gegenüber der Groß- und Kleinschreibung zu erreichen, was bei der Datensuche und -vergleiche nützlich ist.

## Wie geht's:

```gleam
import gleam/string

fn main() {
  let message = "Hallo Welt!"
  let process = string.lower(message)
  process
}  
```

Wenn Sie das obenstehende Gleam-Programm ausführen, erhalten Sie "hallo welt!" als Ausgabe.

## Vertiefung 

Die Umwandlung von Zeichenketten in Kleinbuchstaben ist eine Praxis, die so alt ist wie die Computerprogrammierung selbst. Sie dient dazu, die Unterscheidlichkeit zwischen den Groß- und Kleinschreibungen zu entfernen und dadurch Textsuche und -vergleiche präziser zu machen. 

Alternativen könnten spezifische Abfragen auf der Basis von Groß- oder Kleinschreibung sein, aber das wäre ineffizient und würde den Code komplexer machen.

In Bezug auf die Implementierung verfügt Gleam über die eingebaute Funktion `string.lower`, die diese Aufgabe effizient erledigt. Es ist wichtig zu beachten, dass diese Funktion eine neue Zeichenkette zurückgibt und die original Zeichenkette nicht modifiziert.

## Siehe Auch 

- Gleam’s [Dokumentation für Strings](https://gleam.run/book/tour/strings.html)
- Gleam’s [API-Dokumentation für die `string.lower`](https://hexdocs.pm/gleam_stdlib/Gleam/String.html#lower/1)
- [Gleam Einführung in deutsch](https://gleam.run/getting-started/deutsch/)