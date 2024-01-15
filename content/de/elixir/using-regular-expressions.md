---
title:                "Verwendung regulärer Ausdrücke"
html_title:           "Elixir: Verwendung regulärer Ausdrücke"
simple_title:         "Verwendung regulärer Ausdrücke"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elixir/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Warum

Warum sollte man sich überhaupt mit regulären Ausdrücken in Elixir beschäftigen? Nun, reguläre Ausdrücke ermöglichen es uns, Muster in Zeichenketten zu suchen und zu manipulieren. Dies ist besonders nützlich bei der Verarbeitung von Texten, Suchvorgängen und Datenvalidierung.

## Wie man es macht

Um reguläre Ausdrücke in Elixir zu verwenden, müssen wir das Modul `Regex` importieren. Dann können wir `=~` verwenden, um zu überprüfen, ob ein Muster in einer Zeichenkette vorhanden ist. Zum Beispiel:

```elixir
import Regex

"Hello World" =~ ~r/World/ # Gibt true zurück, da "World" in der Zeichenkette enthalten ist.
```

Wir können auch `capture` verwenden, um Teile einer Zeichenkette zu extrahieren, die mit einem bestimmten Muster übereinstimmen. Zum Beispiel:

```elixir
import Regex

result = capture(~r/(\d+)-(\d+)-(\d+)/, "04-25-2021") # Gibt eine Liste mit den gefangenen Werten zurück.
IO.inspect result # Gibt ["04", "25", "2021"] aus.
```

Wir können auch reguläre Ausdrücke verwenden, um Zeichenketten zu manipulieren. Zum Beispiel können wir `replace` verwenden, um Teilzeichenketten mit einem bestimmten Muster durch einen anderen Wert zu ersetzen. Wir können auch `split` verwenden, um eine Zeichenkette an bestimmten Stellen aufzuteilen. Ein weiteres nützliches Werkzeug ist `match?`, das uns einfach sagen lässt, ob ein Muster in einer Zeichenkette vorhanden ist.

## Tiefere Einblicke

Neben den oben genannten grundlegenden Funktionen gibt es noch viel mehr, was wir mit regulären Ausdrücken in Elixir tun können. Wir können beispielsweise die `Regex`-Module verwenden, um unsere eigenen benannten Muster zu definieren. Wir können auch die `Regex.run`-Funktion verwenden, um Funktionen auf einem Muster anzuwenden, anstatt nur Übereinstimmungen zu finden.

Reguläre Ausdrücke können auch in Kombination mit Mustern und Funktionen in `Enum` und `Stream` verwendet werden, um komplexe String-Manipulationen durchzuführen. Wir können sogar reguläre Ausdrücke verwenden, um Validierungsregeln für Formulare zu schreiben.

Schlussendlich können reguläre Ausdrücke in Elixir sehr mächtig sein, aber sie können auch komplex und manchmal schwer zu lesen sein. Es ist wichtig, das richtige Gleichgewicht zu finden und sie nur zu verwenden, wenn es wirklich notwendig ist.

## Siehe auch

- [Elixir Dokumentation zu regulären Ausdrücken](https://hexdocs.pm/elixir/Regex.html)
- [Elixir Schulungsvideo zu regulären Ausdrücken](https://www.youtube.com/watch?v=uE_KSwh_EVY)
- [Reguläre Ausdrücke in 10 Minuten lernen](https://www.youtube.com/watch?v=EkluES9Rvak)