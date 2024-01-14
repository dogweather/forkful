---
title:                "Elixir: Reguläre Ausdrücke verwenden"
programming_language: "Elixir"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elixir/using-regular-expressions.md"
---

{{< edit_this_page >}}

# Warum

In der Welt der Programmierung gibt es viele Möglichkeiten, um Aufgaben zu lösen. Eine dieser Möglichkeiten sind reguläre Ausdrücke, auch bekannt als Regex. Sie sind eine Mächtige Methode, um Textmuster zu suchen und zu manipulieren. Lassen Sie uns herausfinden, warum es sich lohnt, sich mit regulären Ausdrücken in Elixir zu beschäftigen.

# Wie geht das?

Um reguläre Ausdrücke in Elixir zu nutzen, müssen Sie das Regex-Modul importieren. Dann können Sie verschiedene Funktionen wie `match?`, `replace`, `scan` und `split` verwenden, um Textmuster zu finden, zu ersetzen, zu scannen und zu unterteilen. Sehen wir uns ein Beispiel an:

```Elixir
Regex.match?("Hallo Welt", ~r/Welt/) # gibt true zurück, da "Welt" im Ausdruck enthalten ist
Regex.replace("Hallo Welt", ~r/Welt/, "Elixir") # gibt "Hallo Elixir" aus
Regex.scan("Dieser Text enthält Zahlen wie 12345", ~r/\d+/) # gibt ["12345"] aus
Regex.split("Eine, Zwei, Drei", ~r/, /) # gibt ["Eine", "Zwei", "Drei"] aus
```

# Tiefer eintauchen

Reguläre Ausdrücke können viel komplexere Muster als einfache Texte erkennen. Zum Beispiel können Sie mit dem `|` Operator verschiedene mögliche Ausdrücke definieren und mit `{}` eine Wiederholung angeben. Es gibt auch verschiedene Optionen, die Sie in Ihren regulären Ausdrücken verwenden können, um z.B. Groß- und Kleinschreibung zu ignorieren oder nur ganze Wörter zu suchen. Um mehr über reguläre Ausdrücke zu erfahren, können Sie die offizielle Elixir Dokumentation oder andere Online-Ressourcen nutzen.

# Siehe auch

- [Elixir Dokumentation für das Regex-Modul](https://hexdocs.pm/elixir/Regex.html)
- [Regular-Expressions.info - Eine umfassende Online-Ressource für reguläre Ausdrücke](https://www.regular-expressions.info/)

Mit regulären Ausdrücken können Sie komplexe Textmuster in Elixir problemlos finden und manipulieren. Es lohnt sich, sich in dieses mächtige Werkzeug einzuarbeiten und es in Ihren Projekten einzusetzen. Viel Spaß beim Coden mit regulären Ausdrücken in Elixir!