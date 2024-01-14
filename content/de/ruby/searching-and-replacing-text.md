---
title:                "Ruby: Suchen und Ersetzen von Text"
programming_language: "Ruby"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/ruby/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Warum

Das Suchen und Ersetzen von Text ist eine häufige Aufgabe beim Programmieren. Es hilft dabei, bestimmte Begriffe oder Zeichenfolgen in einem Textdokument oder Code schnell und effizient zu finden und diese durch andere zu ersetzen. Dies kann vor allem dann nützlich sein, wenn man mehrere Dateien oder Zeilen Code auf einmal bearbeiten möchte.

## Wie geht es

Um Text in Ruby zu suchen und zu ersetzen, kann man die `gsub`-Methode verwenden. Diese Methode durchsucht einen String nach einem bestimmten Muster und ersetzt es durch einen angegebenen Wert.

```Ruby
"Hello World".gsub("World", "Ruby") 
# Output: "Hello Ruby"
```

Man kann auch reguläre Ausdrücke verwenden, um gezielt nach bestimmten Mustern zu suchen und diese zu ersetzen.

```Ruby
"Hello 123".gsub(/\d+/, "World")
# Output: "Hello World"
```

## Tiefere Einblicke

Das Suchen und Ersetzen von Text kann auch mit Hilfe von Iterationen und Bedingungen erweitert werden. Durch die Verwendung von Schleifen kann man beispielsweise alle Dateien in einem Verzeichnis durchsuchen und bestimmte Textstellen ersetzen. Auch das Hinzufügen von Ausnahmen für bestimmte Muster oder Dateitypen ist möglich.

## Siehe auch

- [Offizielle Ruby Dokumentation zu `gsub`](https://ruby-doc.org/core-2.7.1/String.html#method-i-gsub)
- [Eine Einführung in reguläre Ausdrücke in Ruby](https://www.rubyguides.com/2015/06/ruby-regex/)
- [Ein praktisches Beispiel für die Anwendung von `gsub` und regulären Ausdrücken](https://medium.com/@miguelmoura/building-a-word-counter-with-regular-expressions-in-ruby-55bcea68e824)