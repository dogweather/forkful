---
title:    "Elixir: String großschreiben"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## Warum

Haben Sie sich jemals gefragt, warum Sie in der Programmierung eine string-Kapitalisierung durchführen sollten? Nun, es gibt mehrere Gründe, warum dies eine wichtige Fähigkeit ist. Zusätzlich zur korrekten Formatierung von Benutzereingaben ist die Konsistenz in Ihrem Code von großer Bedeutung. Außerdem kann die Großschreibung von Strings bei der Sortierung oder beim Vergleich von Daten von Vorteil sein. Lassen Sie uns nun sehen, wie man in Elixir eine Zeichenkette in Großbuchstaben umwandelt.

## Wie funktioniert es

Die Funktionsweise der String-Kapitalisierung in Elixir ist einfach. Zunächst müssen Sie den gewünschten String in einen String-Datentyp umwandeln. Anschließend können Sie die Funktion `String.capitalize/1` aufrufen, um den String in Großbuchstaben umzuwandeln. Schauen wir uns einige Beispiele an:

```Elixir
iex> string = "elixir ist großartig"
"elixir ist großartig"
iex> String.capitalize(string)
"Elixir ist großartig"
```

Wie Sie sehen können, wird der erste Buchstabe des Strings automatisch in einen Großbuchstaben umgewandelt. Wenn Sie möchten, dass alle Wörter im String groß geschrieben werden, können Sie die Funktion `String.upcase/1` verwenden. Zum Beispiel:

```Elixir
iex> String.upcase(string)
"ELIXIR IST GROßARTIG"
```

Wenn Sie jedoch nur den ersten Buchstaben jedes Wortes im String groß schreiben möchten, können Sie die Funktion `String.capitalize_words/1` verwenden. Schauen wir uns ein Beispiel an:

```Elixir
iex> string = "code schreiben ist spaß"
"code schreiben ist spaß"
iex> String.capitalize_words(string)
"Code Schreiben Ist Spaß"
```

## Tiefere Einblicke

Obwohl die `String.capitalize/1` Funktion einfach zu bedienen ist, gibt es noch einige Dinge zu beachten. Zum Beispiel funktioniert die Funktion nicht nur für einzelne Wörter, sondern auch für ganze Sätze und Zeichenketten mit Sonderzeichen. Schauen wir uns dazu einige Beispiele an:

```Elixir
iex> string = "Mein Name ist Elixir!"
"Mein Name ist Elixir!"
iex> String.capitalize(string)
"Mein name ist elixir!"
iex> string = "1,2,3 go!"
"1,2,3 go!"
iex> String.capitalize(string)
"1,2,3 Go!"
```

Wie Sie sehen können, bleibt die Großschreibung von Wörtern wie "ist" oder "go" beim Kapitalisieren erhalten, da diese als Verben betrachtet werden und nicht als Teil des Namens. Wenn Sie jedoch möchten, dass Wörter wie "ist" auch groß geschrieben werden, müssen Sie die Funktion `String.capitalize/2` verwenden und den zweiten Parameter auf `true` setzen. Zum Beispiel:

```Elixir
iex> String.capitalize(string, true)
"1,2,3 Go!"
```

## Siehe auch

- [Die offizielle Elixir Dokumentation zu String-Funktionen](https://hexdocs.pm/elixir/String.html)
- [Elixir School's Guide zur String-Manipulation](https://elixirschool.com/de/lessons/basics/basics-strings/)
- [Elixir-Kapitel im Buch "Sieben Sprachen in sieben Wochen"](https://pragprog.com/titles/btlang/seven-languages-in-seven-weeks/)