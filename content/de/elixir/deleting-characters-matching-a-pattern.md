---
title:                "Elixir: Entfernen von Zeichen, die einem Muster entsprechen"
programming_language: "Elixir"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elixir/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Warum

Das Löschen von Zeichen, die einem bestimmten Muster entsprechen, ist eine nützliche Fähigkeit in der Elixir Programmierung, um verschiedene Arten von Daten effizient zu manipulieren und zu bearbeiten.

## Wie geht's?

Die Elixir-Standardbibliothek bietet die Funktion `String.replace/4`, um Zeichen in einem String durch ein anderes Zeichen zu ersetzen, das einem bestimmten Muster entspricht. Hier ist ein Beispiel, wie diese Funktion verwendet werden kann:

```
Elixir defmodule StringManipulation do
  def delete_characters(str) do
    String.replace(str, ~r/[aeiou]/, "")
  end
end
```

Der obige Code verwendet den regulären Ausdruck `[aeiou]`, um alle Vokale in einem String zu finden und durch ein leeres Zeichen zu ersetzen. 

Wenn wir nun diese Funktion aufrufen und einen String übergeben, erhalten wir das folgende Ergebnis:

```
# StringManipulation.delete_characters("Hello World")
"HllWrld"
```

Dies ist nur ein einfaches Beispiel, wie `String.replace/4` verwendet werden kann, aber es gibt viele weitere Möglichkeiten, diese Funktion in der Praxis einzusetzen. 

## Tiefer tauchen

Für ein tiefes Verständnis des Löschen von Zeichen in Elixir ist es wichtig, die verwendeten regulären Ausdrücke zu verstehen. Dies sind spezielle Zeichenmuster, die verwendet werden, um bestimmte Teile eines Strings zu finden oder zu ersetzen. Elixir verwendet die Syntax von PCRE (Perl Compatible Regular Expressions) für reguläre Ausdrücke und es gibt viele hilfreiche Ressourcen online, um mehr über sie zu erfahren.

Eine weitere wichtige Sache, die es zu beachten gilt, ist, dass `String.replace/4` eine unveränderliche Funktion ist, was bedeutet, dass sie den ursprünglichen String nicht ändert, sondern einen neuen String zurückgibt. Dies ist ein wichtiger Aspekt der funktionalen Programmierung in Elixir und es ist wichtig, dies zu verstehen, um unerwartete Ergebnisse zu vermeiden.

## Siehe auch

- [Elixir Dokumentation zu String.replace/4](https://hexdocs.pm/elixir/String.html#replace/4)
- [PCRE reguläre Ausdrucksbibliothek](https://www.pcre.org/)

Vielen Dank, dass Sie diesen Blogbeitrag zum Löschen von Zeichen in Elixir gelesen haben. Wir hoffen, dass Sie daraus etwas gelernt haben und es in Ihren zukünftigen Elixir-Projekten anwenden können!