---
title:    "Elixir: Großbuchstaben eines Strings"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/elixir/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Warum

Haben Sie sich jemals gefragt, wie man in Elixir eine Zeichenkette (string) großschreibt? Das können Sie ganz einfach tun, indem Sie die `String.capitalize/1` Funktion verwenden. Lesen Sie weiter, um mehr über diese nützliche Funktion zu erfahren.

## Wie geht das?

Um eine Zeichenkette in Elixir großzuschreiben, können Sie die `String.capitalize/1` Funktion verwenden. Sie akzeptiert eine Zeichenkette als Argument und gibt eine neue Zeichenkette zurück, in der der erste Buchstabe jeder einzelnen Wörter großgeschrieben wird. Sehen wir uns ein Beispiel an:

```Elixir
string = "hallo, wie geht es dir?"
String.capitalize(string)
```

Dieser Code gibt `Hallo, Wie Geht Es Dir?` als Ergebnis zurück.

Beachten Sie jedoch, dass die `String.capitalize/1` Funktion nur den ersten Buchstaben jedes einzelnen Wortes großschreibt. Wenn Sie eine Zeichenkette haben, die bereits teilweise oder vollständig großgeschrieben ist, sind die Ergebnisse möglicherweise nicht zufriedenstellend. Sehen wir uns ein weiteres Beispiel an:

```Elixir
string = "hallo, ICH BIN GLÜCKLICH"
String.capitalize(string)
```

Das Ergebnis lautet immer noch `Hallo, Ich Bin Glücklich`, da die Funktion nur den ersten Buchstaben jedes Wortes großschreibt und die anderen Buchstaben unverändert lässt.

## Tiefere Einblicke

Um vollständig zu verstehen, wie die `String.capitalize/1` Funktion funktioniert, sollten Sie wissen, dass sie intern die `String.upcase/1` Funktion verwendet. Diese Funktion wandelt alle Buchstaben einer Zeichenkette in Großbuchstaben um. Dann verwendet die `String.capitalize/1` Funktion die `String.upcase/1` Funktion, um alle Buchstaben in Großbuchstaben umzuwandeln, und wandelt anschließend den Rest der Zeichenkette in Kleinbuchstaben um.

Sie können die `String.upcase/1` Funktion auch direkt verwenden, um alle Buchstaben in Großbuchstaben umzuwandeln. Sehen wir uns ein Beispiel an:

```Elixir
string = "ich bin ein string"
String.upcase(string)
```

Das Ergebnis lautet `ICH BIN EIN STRING`.

## Siehe auch

Weitere Informationen und Beispiele zu Zeichenketten in Elixir finden Sie in der offiziellen [Elixir Dokumentation](https://hexdocs.pm/elixir/String.html).

Wenn Sie sich für fortgeschrittene Möglichkeiten der Zeichenkettenmanipulation in Elixir interessieren, lesen Sie auch unseren Blog-Beitrag über [Pattern Matching in Elixir Strings](https://blog.example.com/pattern-matching-elixir-strings).