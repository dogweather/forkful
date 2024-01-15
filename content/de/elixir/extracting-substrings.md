---
title:                "Extrahieren von Teilzeichenketten"
html_title:           "Elixir: Extrahieren von Teilzeichenketten"
simple_title:         "Extrahieren von Teilzeichenketten"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elixir/extracting-substrings.md"
---

{{< edit_this_page >}}

## Warum 

Es gibt viele Gründe, warum man in der Programmierung Substrings extrahiert. Zum Beispiel kann es hilfreich sein, wenn man Texte analysieren oder manipulieren möchte. Oder wenn man bestimmte Daten aus einer Zeichenkette extrahieren möchte. In diesem Artikel werden wir uns genauer ansehen, wie man mit Elixir Substrings extrahiert.

## Wie geht's 

Um Substrings in Elixir zu extrahieren, können wir die `String.slice/3` Funktion verwenden. Diese Funktion nimmt drei Argumente: Den String, den Startindex und die Länge des Substrings, den wir extrahieren möchten. Schauen wir uns ein Beispiel an:

```Elixir
string = "Hallo, wie geht es dir?"
String.slice(string, 7, 3)
```

Der obige Code wird den Substring "wie" zurückgeben, da das Wort mit dem Index 7 beginnt und eine Länge von 3 Zeichen hat. Das Ergebnis wird in der Konsole ausgegeben.

```
iex> "wie"
```

Wir können auch negative Indizes verwenden, um von hinten nach vorne zu zählen. Zum Beispiel würde `String.slice(string, -3, 3)` den Substring "dir" zurückgeben.

Es gibt auch weitere Funktionen, die wir verwenden können, um Substrings zu extrahieren. `String.split/2` teilt einen String in eine Liste von Substrings auf, basierend auf einem Trennzeichen. `String.replace/4` ersetzt einen angegebenen Teil eines Strings durch einen anderen Substring. Experimentiere mit diesen Funktionen, um zu sehen, wie sie funktionieren.

## Tiefer Einblick 

Die `String.slice/3` Funktion ist eine der vielen nützlichen Funktionen in Elixir, die uns bei der Arbeit mit Zeichenketten helfen. Wenn du tiefer in die Thematik einsteigen möchtest, gibt es noch einige weitere nützliche Funktionen, die in der offiziellen Elixir Dokumentation aufgeführt sind. Hier sind einige Anleitungen, wie man Substrings in Strings auffinden, zählen und ersetzen kann.

## Siehe auch 

- [Offizielle Elixir Dokumentation über Substrings](https://hexdocs.pm/elixir/String.html#slice/3)
- [Weitere nützliche String-Funktionen in Elixir](https://elixirschool.com/de/lessons/basics/string/)
- [Einführung in Elixir-Strings](https://medium.com/elixir-mastery/getting-started-with-elixir-strings-2d9577770d9b)