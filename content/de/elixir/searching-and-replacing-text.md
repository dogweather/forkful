---
title:    "Elixir: Suchen und Ersetzen von Text"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/elixir/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Warum

Es gibt viele Situationen in der Programmierung, in denen wir uns mit der Notwendigkeit konfrontiert sehen, Text zu suchen und zu ersetzen. Dies kann zum Beispiel bei der Überprüfung von Benutzereingaben oder beim Durchsuchen von Dateien sein. In solchen Fällen kann die Verwendung von regulären Ausdrücken sehr hilfreich sein, um die gewünschten Textfragmente zu finden und zu ändern.

## Wie geht das?

In Elixir gibt es verschiedene Methoden, um Text zu durchsuchen und zu ersetzen. Eine davon ist die Verwendung der Funktion `String.replace/4`. Diese Funktion erwartet vier Argumente: den zu durchsuchenden Text, das zu suchende Muster, das zu ersetzende Muster und optional noch ein Flag, um die Suchanfrage zu beeinflussen. Hier ist ein Beispiel, wie man damit einen einfachen Such- und Ersatzvorgang durchführen kann:

```Elixir
text = "Hallo, Welt!"
new_text = String.replace(text, "Hallo", "Hi")
IO.puts new_text  # Ausgabe: Hi, Welt!
```

Zusätzlich zur Verwendung von Zeichenketten können in Elixir auch reguläre Ausdrücke verwendet werden. Hierfür steht die Funktion `Regex.replace/4` zur Verfügung. Sie funktioniert ähnlich wie `String.replace/4`, erwartet aber als zweites Argument einen regulären Ausdruck anstelle einer Zeichenkette. Hier ist ein Beispiel:

```Elixir
text = "Der Himmel ist blau!"
new_text = Regex.replace(text, ~r/blau/, "rot")
IO.puts new_text  # Ausgabe: Der Himmel ist rot!
```

## Tiefergehende Informationen

In beiden obigen Beispielen wurde nur der erste Treffer innerhalb des Textes ersetzt. Wenn man alle Vorkommnisse ersetzen möchte, gibt es verschiedene Möglichkeiten, dies zu erreichen. Zum Beispiel kann das Flag `global: true` verwendet werden, um die Suche auf den gesamten Text auszuweiten. Oder man kann die Funktion `Regex.replace_all/4` verwenden. Es gibt auch noch viele weitere Funktionen und Optionen, um die Suche und den Ersatz von Texten in Elixir zu manipulieren. Es empfiehlt sich, die offizielle Elixir-Dokumentation zu diesem Thema zu konsultieren, um mehr darüber zu erfahren.

## Siehe auch

- [String-Modul in der Elixir-Dokumentation](https://hexdocs.pm/elixir/String.html)
- [Reguläre Ausdrücke in der Elixir-Dokumentation](https://hexdocs.pm/elixir/Regex.html)