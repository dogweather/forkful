---
title:    "Elixir: Das Löschen von Zeichen, die einem Muster entsprechen"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

# Warum

Das Löschen von Zeichen, die einem bestimmten Muster entsprechen, kann sehr nützlich sein, wenn man Daten filtern oder bearbeiten muss. Dies kann beispielsweise bei der Verarbeitung von Benutzereingaben oder beim Entfernen von unnötigen Zeichen aus Texten hilfreich sein.

# Wie geht das?

Es gibt verschiedene Möglichkeiten, in Elixir Zeichen zu löschen, die einem bestimmten Muster entsprechen. Im Folgenden werden drei Methoden vorgestellt:

## String.replace/4

Eine Möglichkeit ist die Verwendung von `String.replace/4`. Diese Funktion nimmt als Argumente den zu durchsuchenden String, das zu ersetzende Muster, die Ersatzzeichena und optional auch eine Anzahl von Vorkommnissen, die ersetzt werden sollen. Hier ein Beispiel:

```Elixir
string = "Dies ist ein Beispieltext mit vielen Zahlen 123 und Sonderzeichen!@#$"
String.replace(string, ~r/\d+/, "")
```

Das Ergebnis wird gleichzeitig zurückgegeben und in der Variablen `string` gespeichert. Die Ausgabe lautet `Dies ist ein Beispieltext mit vielen Zahlen  und Sonderzeichen!@#$`, da alle Zahlen im Text entfernt wurden.

## Regex.replace/3

Eine weitere Möglichkeit ist die Verwendung von `Regex.replace/3`. Diese Funktion erwartet ein Regex-Muster, den zu durchsuchenden String und die Ersatzzeichen. Hier ein Beispiel:

```Elixir
string = "Ich mag Elixir, weil es so vielseitig ist!"
Regex.replace(~r/Elixir/, string, "Phoenix")
```

Die Ausgabe lautet `Ich mag Phoenix, weil es so vielseitig ist!`, da das Wort "Elixir" durch "Phoenix" ersetzt wurde.

## String.replace_leading/4

Die dritte Option ist die Verwendung von `String.replace_leading/4`, um nur die führenden Vorkommnisse des Musters zu ersetzen. Diese Funktion funktioniert ähnlich wie `String.replace/4`, kann aber auch mit verschachtelten Mustern umgehen. Hier ein Beispiel:

```Elixir
string = "Dies$&var; lässt sich leicht löschen!"
String.replace_leading(string, "$&var;", "")
```

Die Ausgabe lautet `Dies lässt sich leicht löschen!`, da das Muster "$&var;" am Anfang des Strings entfernt wurde.

# Tief tauchen

Um Zeichen in Elixir effektiv zu löschen, ist es wichtig, sich mit Regex-Mustern vertraut zu machen und die verschiedenen Funktionen wie `String.replace/4` und `Regex.replace/3` zu verstehen. Auch das Verständnis von verschachtelten Mustern und deren Umgang mit `String.replace_leading/4` kann dabei hilfreich sein.

# Siehe auch

- [Elixir Dokumentation über Zeichenersetzung](https://elixir-lang.org/getting-started/pattern-matching.html#string-replacement)
- [Regex Tutorial auf Elixir School](https://elixirschool.com/de/lessons/basics/pattern-matching/#regex)