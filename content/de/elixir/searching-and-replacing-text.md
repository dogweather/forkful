---
title:    "Elixir: Suchen und Ersetzen von Text"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## Warum

Das Durchsuchen und Ersetzen von Text ist eine grundlegende Aufgabe für Programmiererinnen und Programmierer. Es ermöglicht uns, schnell und effizient bestimmte Muster oder Wörter in unserem Code zu ändern oder zu ersetzen. In diesem Blog-Beitrag werden wir uns ansehen, wie wir diese Aufgabe in Elixir lösen können.

## How To

Elixir bietet mehrere Möglichkeiten, um Text zu durchsuchen und zu ersetzen. Hier sind einige Beispiele:

### Suchen und Ersetzen von Text mit `String.replace/4`

Wir können die Funktion `String.replace/4` verwenden, um einen bestimmten Text in einer Zeichenkette zu suchen und durch einen anderen Text zu ersetzen. Hier ist ein Beispiel:

```Elixir
iex> String.replace("Hallo Welt", "Welt", "Elixir")
"Hallo Elixir"
```

Diese Funktion akzeptiert vier Argumente: Die ursprüngliche Zeichenkette, den zu ersetzenden Text, den Ersatztext und eine Option `count`, die die maximale Anzahl der Vorkommen des zu ersetzenden Textes angibt.

### Verwenden von regulären Ausdrücken mit `Regex.replace/3`

Wir können auch reguläre Ausdrücke verwenden, um komplexe Muster zu durchsuchen und zu ersetzen. Die Funktion `Regex.replace/3` nimmt eine reguläre Ausdruckszeichenkette, den zu ersetzenden Text und den Ersatztext als Argumente. Hier ist ein Beispiel, wie wir alle Vorkommen von Zahlen in einer Zeichenkette ersetzen können:

```Elixir
iex> Regex.replace("Das Buch hat 100 Seiten", ~r/[0-9]+/, "drei")
"Das Buch hat drei Seiten"
```

### Verwendung von Mustern mit `String.replace_pattern/3`

Für noch komplexere Muster können wir auch die Funktion `String.replace_pattern/3` verwenden, die ähnlich wie `Regex.replace/3` arbeitet, aber anstelle eines regulären Ausdrucks ein Muster verwendet. Hier ist ein Beispiel, um alle Wörter zu ersetzen, die mit "E" beginnen:

```Elixir
iex> String.replace_pattern("Elixir ist eine großartige Sprache", "E[a-z]+", "Ruby")
"Ruby ist eine großartige Sprache"
```

## Deep Dive

Elixir bietet noch mehr Funktionen zum Durchsuchen und Ersetzen von Text, wie z.B. `String.split/2`, `String.replace_leading/3` und `String.replace_trailing/3`. Wir empfehlen Ihnen, die offizielle Dokumentation von Elixir für mehr Informationen zu diesen Funktionen zu lesen.

## Siehe auch

* Offizielle Elixir Dokumentation: https://hexdocs.pm/elixir/String.html#replace/4
* Reguläre Ausdrücke in Elixir: https://hexdocs.pm/elixir/Regex.html