---
title:    "Elixir: Verwendung von regulären Ausdrücken"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## Warum

Elixir ist eine leistungsstarke Programmiersprache, die immer beliebter wird. Einer der Gründe dafür ist, dass es eine einfache und elegante Möglichkeit bietet, reguläre Ausdrücke zu verwenden. Reguläre Ausdrücke sind nützlich, um Textmuster in Strings zu erkennen und zu bearbeiten. Wenn du Elixir lernst oder bereits ein erfahrener Benutzer bist, solltest du auf jeden Fall die Verwendung regulärer Ausdrücke in Betracht ziehen.

## Wie geht's?

Die Syntax für reguläre Ausdrücke in Elixir verwendet ähnliche Konstruktionen wie viele andere Sprachen. Grundsätzlich gibt es zwei Arten, reguläre Ausdrücke in Elixir zu verwenden: mit dem `=~` Operator oder mit der Funktion `Regex.match?()`. Hier sind Beispiele für beide Methoden:

```
Elixir Code für =~ Operator

string = "Ich liebe Elixir"
regex = ~r/Elixir/

# Überprüfe, ob der String den regulären Ausdruck enthält
string =~ regex
# Ausgabe: true
```

```
Elixir Code für Regex.match?() Funktion

string = "Hallo Welt!"
regex = ~r/Welt/

# Überprüfe, ob der String den regulären Ausdruck enthält
Regex.match?(regex, string)
# Ausgabe: true
```

Wie du sehen kannst, können reguläre Ausdrücke recht einfach in Elixir verwendet werden. Du kannst auch Optionen wie `i` für eine Case-Insensitivität oder `m` für Multiline-Strings hinzufügen.

## Tief eintauchen

Ähnlich wie bei anderen Programmiersprachen kannst du in Elixir auch Gruppen von Text in regulären Ausdrücken definieren und diese mithilfe von Capture Groups erfassen. Du kannst diese Gruppen dann in anderen Teilen deines Codes verwenden. Hier ist ein Beispiel dafür:

```
Elixir Code für Capture Groups

string = "Magst du Kaffee oder Tee?"
regex = ~r/Kaffee oder Tee\?/

# Erfasse "Kaffee" oder "Tee" und speichere es in einer Gruppe
Regex.named_captures(regex, string)["option"] 
# Ausgabe: Kaffee
```

Ein weiteres nützliches Tool, das in Elixir angeboten wird, sind sogenannte "Backreferences". Diese erlauben es dir, bereits erfasste Textmuster in deinem regulären Ausdruck für weitere Vergleiche zu verwenden. Hier ist ein Beispiel dafür:

```
Elixir Code für Backreferences

string = "Heute ist der 12. März"
regex = ~r/(\d{1,2})\. März/

# Erfasse das Datum als Zahl und Text und gib beide Werte aus
Regex.named_captures(regex, string)["day"] # Ausgabe: 12
Regex.named_captures(regex, string)["month"] # Ausgabe: März
```

Dies ist natürlich nur ein kleiner Einblick in die Möglichkeiten, die Elixir in Bezug auf reguläre Ausdrücke bietet. Wenn du mehr erfahren möchtest, empfehle ich dir, die offizielle Elixir-Dokumentation zu lesen oder weitere Online-Ressourcen zu nutzen.

## Siehe auch

- [Offizielle Elixir-Dokumentation zu regulären Ausdrücken](https://elixir-lang.org/getting-started/modules-and-functions.html#regular-expressions)
- [ElixirSchool Tutorial zu regulären Ausdrücken](https://elixirschool.com/en/lessons/advanced/pattern-matching/)
- [Regular Expressions 101 - interaktiver Online-Regex-Tester](https://regex101.com/)