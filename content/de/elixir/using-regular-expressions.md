---
title:    "Elixir: Verwendung von regulären Ausdrücken"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/elixir/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Warum
Reguläre Ausdrücke sind ein mächtiges Werkzeug in der Elixir-Programmierung, das dir helfen kann, komplexe Muster in Strings zu finden und zu verarbeiten. Sie ermöglichen es dir, effizienter und präziser zu arbeiten und können auch bei der Validierung von Benutzereingaben hilfreich sein.

## Wie verwende ich reguläre Ausdrücke in Elixir
Die Verwendung von regulären Ausdrücken in Elixir ist einfach und ähnlich wie in anderen Programmiersprachen. Zunächst muss das `Regex`-Modul in deinem Code importiert werden. Du kannst dann den gewünschten Ausdruck mit Hilfe von `~r//` und `~r//i` (für eine Fall-insensitive Suche) definieren. Hier ist ein Beispiel, das alle Wörter in einem Satz sucht, die mit einem Großbuchstaben beginnen:

```Elixir
import Regex

sentence = "Das ist ein Beispiel Satz."
match = ~r/[A-Z]\w+/.match(sentence)
```

Das Ergebnis dieses Beispiels ist `{:ok, "Das"}`. Beachte, dass `match` ein `Tuple` zurückgibt, das `{:ok, value}` enthält, wenn ein Übereinstimmung gefunden wurde, oder `{:error, message}`, wenn keine Übereinstimmung gefunden wurde.

## Eine tiefere Einblicke in die Verwendung von regulären Ausdrücken in Elixir
Reguläre Ausdrücke in Elixir sind nicht nur auf die simple Verwendung von `match` beschränkt. Du kannst auch `Regex.replace/3` verwenden, um Zeichenketten zu ersetzen, `Regex.split/2` um Strings an bestimmten Mustern zu teilen und `Regex.scan/2` um alle übereinstimmenden Teile eines Strings zu extrahieren.

Es gibt auch verschiedene Metazeichen und Quantifier, die in regulären Ausdrücken verwendet werden können, um komplexere Ausdrücke zu definieren. Eine vollständige Liste findest du in der offiziellen [Elixir-Dokumentation](https://hexdocs.pm/elixir/Regex.html).

## Siehe auch
- Die offizielle Elixir-Dokumentation für das `Regex`-Modul: https://hexdocs.pm/elixir/Regex.html
- Ein Tutorial zum Thema reguläre Ausdrücke in Elixir: https://elixirschool.com/de/lessons/advanced/regex/