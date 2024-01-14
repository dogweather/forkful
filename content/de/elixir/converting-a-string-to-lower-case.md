---
title:                "Elixir: Umwandlung eines Strings in Kleinbuchstaben"
programming_language: "Elixir"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elixir/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# Warum

Wenn Sie regelmäßig mit Zeichenketten (Strings) in Ihrer Elixir-Programmierung arbeiten, könnte es nützlich sein, zu wissen, wie man eine Zeichenkette in Kleinbuchstaben umwandelt. Dies kann Ihnen dabei helfen, Eingaben von Benutzern zu vereinheitlichen oder Datenbankabfragen zu erleichtern.

# Wie Sie eine Zeichenkette in Kleinbuchstaben umwandeln

```Elixir
string = "HALLO WELT"
IO.puts String.downcase(string)
```
Output:
"hallo welt"

In diesem Beispiel haben wir die Funktion `String.downcase/1` verwendet, um die Zeichenkette `string` in Kleinbuchstaben umzuwandeln. Diese Funktion akzeptiert eine Zeichenkette als Argument und gibt eine neue Zeichenkette zurück, die alle Großbuchstaben in Kleinbuchstaben umgewandelt hat.

Ein weiterer Weg, eine Zeichenkette in Kleinbuchstaben umzuwandeln, ist die Verwendung der `String.downcase/2` Funktion. Diese Funktion akzeptiert zwei Parameter, wobei der zweite optional ist. Hier ist ein Beispiel:

```Elixir
string = "DIES IST EIN BEISPIEL"
IO.puts String.downcase(string, "de")
```

Output:
"dies ist ein beispiel"

In diesem Beispiel haben wir den optionalen Parameter `"de"` angegeben, um anzugeben, dass die Zeichenkette im deutschen Alphabet vorliegt. Dadurch werden beispielsweise Umlaute korrekt umgewandelt.

# Tiefgehender Einblick

Elixir verwendet intern Unicode, um Zeichenketten zu repräsentieren. Dies bedeutet, dass die Funktionen `String.downcase/1` und `String.downcase/2` nach denselben Regeln arbeiten, die auch für Unicode gelten. Dies bedeutet, dass zum Beispiel auch griechische oder kyrillische Buchstaben in Kleinbuchstaben umgewandelt werden können.

Es ist auch wichtig, zu beachten, dass die Funktionen `String.downcase/1` und `String.downcase/2` keine Seiteneffekte haben. Dies bedeutet, dass sie die ursprüngliche Zeichenkette nicht ändern werden, sondern stattdessen eine neue umgewandelte Zeichenkette zurückgeben.

# Siehe auch

- [Elixir String-Dokumentation](https://hexdocs.pm/elixir/String.html)
- [Erlang Unicode-Dokumentation](https://erlang.org/doc/man/unicode.html)