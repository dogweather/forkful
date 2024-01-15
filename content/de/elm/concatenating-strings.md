---
title:                "Zeichenketten verbinden"
html_title:           "Elm: Zeichenketten verbinden"
simple_title:         "Zeichenketten verbinden"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elm/concatenating-strings.md"
---

{{< edit_this_page >}}

## Warum

Manchmal muss man in der Programmierung einzelne Textfragmente zu einem ganzen Satz zusammenfügen. Diesen Vorgang nennt man "String-Konkatenation". Es ist eine nützliche Fähigkeit, die beim Schreiben von Texten oder der Erstellung von dynamischen Inhalten für Webseiten sehr hilfreich sein kann.

## Wie es geht

Um Strings in Elm zusammenzufügen, kann man den Operatoren "+" oder "++" verwenden. "+" fügt zwei Strings zusammen und "++" verbindet beliebig viele Strings miteinander.

```Elm
"Hello" ++ " World" -- Ausgabe: "Hello World"
"Nummer " ++ "42" -- Ausgabe: "Nummer 42"
"Mein Name ist " ++ "Max" ++ " und ich bin " ++ "25" ++ " Jahre alt" -- Ausgabe: "Mein Name ist Max und ich bin 25 Jahre alt"
```

Wenn man jedoch eine größere Anzahl von Strings zusammenfügen muss, empfiehlt es sich, die Funktion "String.concat" zu verwenden. Diese Funktion erwartet eine Liste von Strings und fügt sie alle zusammen.

```Elm
String.concat ["Ich", "bin", "ein", "String"] -- Ausgabe: "IchbineinString"
```

Eine andere Möglichkeit ist die Verwendung von "String.join", wo man zusätzlich ein Trennzeichen zwischen den einzelnen Elementen angeben kann.

```Elm
String.join "-" ["Elm", "Programmierung", "Artikel"] -- Ausgabe: "Elm-Programmierung-Artikel"
```

## Tief einsteigen

Beim Zusammenfügen von Strings in Elm sollte man beachten, dass es sich bei Strings um unveränderliche Datentypen handelt. Das bedeutet, dass der ursprüngliche String nicht verändert wird, sondern ein neuer String mit dem zusammengesetzten Inhalt erstellt wird.

Außerdem ist zu beachten, dass die Reihenfolge beim Zusammenfügen von Strings wichtig ist. Verwendet man "+" oder "++", werden die Strings von links nach rechts zusammengefügt. Bei der Verwendung von "String.concat" oder "String.join" ist die Reihenfolge jedoch umgekehrt, da die Liste von Strings von hinten aufgerollt wird.

## Siehe auch

- [Die offizielle Dokumentation zu Strings in Elm](https://package.elm-lang.org/packages/elm/core/latest/String)
- [Ein einfaches Beispiel zur String-Konkatenation in Elm](https://elmprogramming.com/string-concatenation.html)
- [Weitere Tipps und Tricks zur effizienten Verwendung von Strings in Elm](https://www.technologies-ebusiness.com/tech/concatenate-strings-dynamically-in-elm-programming.html)