---
title:                "Ruby: Löschen von Zeichen, die einem Muster entsprechen"
programming_language: "Ruby"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/ruby/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Warum

Das Löschen von Zeichen in einem Text oder einer Zeichenfolge, die einem bestimmten Muster entsprechen, kann eine nützliche Fähigkeit in der Ruby-Programmierung sein. Es kann helfen, Textdaten zu bereinigen oder spezifische Teile des Textes zu entfernen, die nicht benötigt werden. In diesem Blog-Artikel werden wir uns ansehen, wie man Zeichen, die einem bestimmten Muster entsprechen, in Ruby löschen kann.

## Wie es geht

Um Zeichen in Ruby zu löschen, die einem bestimmten Muster entsprechen, verwenden wir die `delete`-Methode. Diese Methode akzeptiert ein Argument, das die Zeichen enthält, die gelöscht werden sollen, sowie optional ein zweites Argument, das das Ersatzzeichen angibt, das anstelle der gelöschten Zeichen verwendet werden soll.

Ein Beispiel könnte folgendermaßen aussehen:

```
text = "Hallo Welt!"
puts text.delete("l") # Output: Hao We!
puts text.delete("l", "LL") # Output: HaLLo WeLLt!
```

In diesem Beispiel löschen wir alle "l"-Zeichen aus dem Text und ersetzen sie mit "LL". Die `delete`-Methode kann auch mit regulären Ausdrücken verwendet werden, um komplexere Muster zu löschen.

## Ein tieferer Einblick

Um die `delete`-Methode besser zu verstehen, werfen wir einen Blick auf ihre Funktionsweise im Hintergrund. Diese Methode basiert auf der `tr`-Methode, die das Ersetzen von Zeichen durch andere Zeichen ermöglicht. Wenn wir also `delete` verwenden, führt Ruby im Grunde genommen dieselben Schritte aus wie bei der Verwendung von `tr`, mit dem Unterschied, dass wir die Ersatzzeichen nicht angeben, sondern sie einfach löschen.

Ein weiteres wichtiges Detail ist, dass die `delete`-Methode nicht nur auf Strings, sondern auch auf Arrays angewendet werden kann. In diesem Fall werden alle Elemente gelöscht, die dem angegebenen Muster entsprechen.

## Siehe auch

* [Ruby String Dokumentation](https://ruby-doc.org/core-3.0.0/String.html)
* [Reguläre Ausdrücke in Ruby](https://www.rubyguides.com/2015/06/ruby-regex/)
* [Ruby Array Dokumentation](https://ruby-doc.org/core-3.0.0/Array.html)