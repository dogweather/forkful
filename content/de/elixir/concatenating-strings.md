---
title:                "Elixir: Zeichenketten verknüpfen"
simple_title:         "Zeichenketten verknüpfen"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elixir/concatenating-strings.md"
---

{{< edit_this_page >}}

## Warum

Das Verketten von Strings ist eine grundlegende Operation in der Programmierung, die es ermöglicht, Texte dynamisch zu erstellen. Es kann besonders nützlich sein, wenn Daten aus verschiedenen Quellen zusammengeführt werden müssen, um eine aussagekräftige Ausgabe zu erzeugen. In Elixir ist die Konkatenation von Strings eine einfache und effiziente Methode, um diese Aufgabe zu erledigen.

## Wie geht's

Um Strings in Elixir zu verketten, verwenden wir einfach den Operator `<>`. Dieser nimmt zwei Strings als Operanden und gibt einen neuen String zurück, der die beiden kombiniert. Hier ist ein Beispielcode:

```Elixir
name = "Max"
greeting = "Hallo "
output = greeting <> name

IO.puts(output) # Ausgabe: Hallo Max
```

Hier sehen wir, dass wir den Namen "Max" an den Gruß "Hallo " anhängen und den daraus resultierenden String der Variable `output` zuweisen. Wir können auch mehr als zwei Strings auf diese Weise verketten:

```Elixir
one = "1"
two = "2"
three = "3"

result = one <> two <> three
IO.puts(result) # Ausgabe: 123
```

Beachten Sie, dass die Reihenfolge der Strings hier wichtig ist. Eine andere Reihenfolge würde zu einem anderen Ergebnis führen. Wir können auch Variablen in der Verkettungsoperation verwenden:

```Elixir
prefix = "The number is "
number = 42

result = prefix <> number
IO.puts(result) # Ausgabe: The number is 42
```

## Tiefer Einblick

In Elixir sind Strings tatsächlich Listen von Codepoints. Das bedeutet, dass sie als Arrays von Zahlen dargestellt werden, die die Unicode-Werte der einzelnen Zeichen im String darstellen. Wenn wir also zwei Strings verketten, fügt der `<>` Operator einfach die beiden Lists zusammen. Dies ist ein schneller und effizienter Prozess, da die Listen im Arbeitsspeicher nicht neu allokiert werden müssen.

Es ist auch erwähnenswert, dass in Elixir Strings in UTF-8 kodiert sind, was bedeutet, dass sie Unicode-Unterstützung bieten und somit Zeichen in anderen Sprachen problemlos verarbeiten können.

In einigen Fällen kann es sinnvoll sein, Strings mit einer anderen Methode als der Verkettung zu kombinieren. Dafür gibt es die `Kernel.to_string/1` Funktion, die einen beliebigen Wert in einen String konvertiert. Hier ein Beispiel:

```Elixir
total = 50
result = "Das Ergebnis ist: " <> Kernel.to_string(total)
IO.puts(result) # Ausgabe: Das Ergebnis ist: 50
```

## Siehe auch

- [Elixir Dokumentation für Strings](https://hexdocs.pm/elixir/String.html)
- [Offizielle Elixir Webseite](https://elixir-lang.org/)
- [Einführung in die Programmierung mit Elixir (auf Deutsch)](https://www.tutorials.de/threads/einfuehrung-in-die-programmierung-mit-elixir.419657/)