---
title:                "Die Länge eines Strings ermitteln"
html_title:           "Java: Die Länge eines Strings ermitteln"
simple_title:         "Die Länge eines Strings ermitteln"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elm/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?
Die Länge eines Strings zu finden bedeutet, die Anzahl der Zeichen darin zu zählen. Programmierer tun dies in Fällen, in denen sie die Texteingabe kontrollieren oder Daten validieren müssen.

## Wie geht's:
In Elm ist es einfach, die Länge eines Strings zu finden. Alles was Sie brauchen ist die eingebaute Funktion `String.length`.

```Elm
import String

main =
    String.length "Hallo Welt!" --> Ausgabe ist 12
```
In diesem Code ist "Hallo Welt!" unser String und das Ergebnis ist 12 (da es 12 Zeichen enthält, einschließlich Leerzeichen und Ausrufezeichen).

## Tiefere Tauchgänge
Schon seit den Anfängen der Programmierung sind Strings und ihre Manipulationen wie das Finden der Länge ein grundlegender Aspekt. In früheren Versionen von Programmiersprachen war das Zählen der Zeichen nicht immer so einfach, oft mussten wir durch jeden Buchstaben des Strings iterieren.

In Bezug auf Alternativen könnten Sie eine benutzerdefinierte Funktion schreiben, die die Zeichen einzeln zählt. Allerdings ist die Verwendung der eingebauten `String.length` Funktion effizienter und macht Ihren Code sauberer.

Bezüglich der Implementierung verwendet Elm interne Algorithmen zur Optimierung der String-Längenbestimmung. Im Grunde wird jedes Zeichen im String gezählt, aber aufgrund von Optimierungen geschieht dies schnell und effizient.

## Siehe auch
Für weitergehende Studien empfehle ich diese Quellen:

* Elm Dokumentation: String - https://package.elm-lang.org/packages/elm/core/latest/String
* Elm Diskussionsforum: Verwendung von String Funktionen - https://discourse.elm-lang.org/
* Buch: Programmieren in Elm - https://pragprog.com/titles/jfelm/programming-elm/

Bitte seien Sie sich bewusst, dass diese Artikel jeweils auf Englisch sind.