---
title:                "Haskell: Verwendung von regulären Ausdrücken"
simple_title:         "Verwendung von regulären Ausdrücken"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/haskell/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Warum

Reguläre Ausdrücke sind ein leistungsstarkes Werkzeug für die Mustererkennung und -manipulation in Haskell. Sie können verwendet werden, um komplexe Such- und Ersetzungsvorgänge in Strings durchzuführen, was die Programmierung effizienter und einfacher macht.

## Wie man es benutzt

Um reguläre Ausdrücke in Haskell zu verwenden, muss das `Text.Regex`-Modul importiert werden. Eine einfache Möglichkeit, einen regulären Ausdruck zu erstellen, ist die Verwendung von sogenannten *string literals* in Haskell. Diese werden mit doppelten Anführungszeichen eingefasst und mit einem `~` vorangestellt, um sie als regulären Ausdruck zu kennzeichnen.

Hier ist ein einfaches Beispiel, um alle Vokale in einem String zu entfernen:

```haskell
import Text.Regex

noVowels :: String -> String
noVowels str = subRegex (mkRegex "[aeiou]") str ""
```

Die Funktion `mkRegex` erstellt einen regulären Ausdruck, der alle Vokale enthält, und `subRegex` ersetzt alle Vorkommen dieser Vokale in `str` durch einen leeren String.

Für komplexere Muster können auch reguläre Ausdrücke mit der Funktion `makeRegex` erstellt werden. Diese erlaubt es, die Suchparameter und Optionen genauer anzugeben.

## Tiefentauchen

Reguläre Ausdrücke verwenden einen speziellen Syntax, um Muster zu definieren und zu erweitern. Hier sind einige grundlegende Beispiele für das Erstellen von Mustern:

- `.`: Beliebiges Zeichen (außer Zeilenumbrüche)
- `[]`: Zeichenklasse - gibt an, welche Zeichen in die Lücke passen
- `+`: Ein oder mehr Vorkommen des vorhergehenden Zeichens
- `*`: Null oder mehr Vorkommen des vorhergehenden Zeichens
- `?`: Null oder ein Vorkommen des vorhergehenden Zeichens
- `|`: Oder - gibt zwei oder mehr Alternativen an

Es gibt noch viele weitere Symbole und Möglichkeiten, reguläre Ausdrücke zu gestalten, aber diese Basics reichen oft aus, um einfache Aufgaben zu erledigen.

## Siehe auch

- [Hackage-Dokumentation zu Text.Regex](https://hackage.haskell.org/package/regex/docs/Text-Regex.html)
- [Reguläre Ausdrücke in 5 Minuten](https://www.regular-expressions.info/tutorial.html)
- [Hoogle-Suche nach regulären Ausdrücken in Haskell](https://www.haskell.org/hoogle/?hoogle=regex)