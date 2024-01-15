---
title:                "HTML-Parsing"
html_title:           "Elixir: HTML-Parsing"
simple_title:         "HTML-Parsing"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elixir/parsing-html.md"
---

{{< edit_this_page >}}

## Warum

Warum sollte jemand Interesse an der Analyse von HTML haben? Nun, HTML ist die grundlegendste Sprache, die verwendet wird, um Webseiten zu erstellen. Das Verständnis von HTML ist daher für jeden, der sich mit Webentwicklung beschäftigt, unerlässlich.

## Wie geht man vor

Um HTML in Elixir zu analysieren, gibt es verschiedene Bibliotheken zur Auswahl. Eine beliebte Wahl ist Floki, die es ermöglicht, mit DOM-ähnlichen Strukturen zu arbeiten. Schauen wir uns an, wie wir mit Hilfe von Floki eine HTML-Datei analysieren und bearbeiten können.

```Elixir
# Zuerst installieren wir Floki über den Hex Package Manager
mix deps.get floki

# Dann importieren wir die Bibliothek in unser Modul
import Floki

# Anschließend können wir eine HTML-Datei laden und sie mit Floki analysieren
html = File.read!("beispiel.html")
parsed_html = Floki.parse(html)

# Wir können jetzt mithilfe von Floki nach bestimmten Elementen suchen, zum Beispiel nach Überschriften
headlines = Floki.find(parsed_html, "h1")

# Und schließlich können wir das Ergebnis ausgeben
IO.puts(headlines)
```

Die Ausgabe wird alle Überschriften in der HTML-Datei beinhalten, die mit `<h1>` gekennzeichnet sind.

## Tiefergehende Informationen

Floki basiert auf dem CSS-Selektor-Modell, was bedeutet, dass wir nach Elementen suchen können, indem wir den Selektor angeben, den wir auch in CSS verwenden würden. Beispiele dafür wären `.class` für Klassen, `#id` für IDs und `a` für Link-Elemente.

Es gibt auch andere Bibliotheken wie HParse oder Dexter, die sich mehr auf die Extraktion bestimmter Inhalte aus einer HTML-Datei konzentrieren. Sie können auch in Elixir verwendet werden, um HTML zu analysieren, aber Floki ist eine gute Wahl für allgemeinere Aufgaben.

## Siehe auch

- [Floki Dokumentation](https://hexdocs.pm/floki/)
- [HParse](https://github.com/myiesh/HParse)
- [Dexter](https://github.com/elixir-casts/dexter)