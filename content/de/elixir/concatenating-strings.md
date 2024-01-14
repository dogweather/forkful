---
title:                "Elixir: Zeichenfolgen verbinden"
programming_language: "Elixir"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elixir/concatenating-strings.md"
---

{{< edit_this_page >}}

## Warum

In der Welt der Programmierung gibt es viele Methoden, um Strings miteinander zu verbinden. Warum sollte man also Elixir verwenden, um Strings zu verketten? Die Antwort ist einfach: Elixir bietet eine benutzerfreundliche und effiziente Syntax, die es Entwicklern ermöglicht, schnell und einfach String-Konkatenation durchzuführen.

## Wie man Strings in Elixir verketten kann

Um Strings in Elixir zu verketten, gibt es zwei entscheidende Methoden: die Verwendung des "String." Moduls und das Verketten mit dem "++" Operator.

Eine Möglichkeit, Strings zu verketten, ist die Verwendung des "String." Moduls. Hier ist ein Beispielcode mit dem "String.concat/2" Befehl:

```Elixir
first_name = "Max"
last_name = "Müller"

full_name = String.concat(first_name, last_name)
```

Die Ausgabe des obigen Codes wäre "MaxMüller". 

Eine weitere Möglichkeit, Strings zu verketten, ist die Verwendung des "++" Operators. Hier ist ein Beispielcode:

```Elixir
first_name = "Max"
last_name = "Müller"

full_name = first_name ++ last_name
```

Die Ausgabe hier wäre auch "MaxMüller".

## Tiefere Einblicke

Eine wichtige Sache zu beachten ist, dass beide Methoden nur mit String-Typen funktionieren. Wenn Sie also versuchen, andere Datentypen wie Integer oder Floats zu verketten, erhalten Sie einen Fehler. 

Außerdem kann das "++" Zeichen auch verwendet werden, um mehrere Strings miteinander zu verketten, ohne dass das "String." Modul benötigt wird. Hier ist ein Beispiel:

```Elixir
full_name = "Max" ++ " " ++ "Müller"
```

Die Ausgabe hier ist "Max Müller".

## Siehe auch

- Elixir Dokumentation zum "String." Modul: https://hexdocs.pm/elixir/String.html
- Elixir Dokumentation zu Operatoren: https://elixir-lang.org/getting-started/operators.html
- Eine kurze Einführung in das Konkatenieren von Strings in Elixir: https://blog.appsignal.com/2017/07/17/elixir-alchemy-concatenating-strings.html