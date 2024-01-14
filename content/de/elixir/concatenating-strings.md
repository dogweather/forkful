---
title:    "Elixir: Verbinden von Zeichenketten"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## Warum

Das Verbinden von Strings ist ein wesentlicher Teil der Programmierung in Elixir. Durch die Kombination von Zeichenketten können komplexere Textstrukturen erstellt werden, die für bestimmte Anwendungen unerlässlich sind.

## Wie

Die Verkettung von Strings ist in Elixir ganz einfach und kann auf verschiedene Arten durchgeführt werden. Die grundlegende Syntax sieht wie folgt aus:

```Elixir
string1 <> string2
```

Dies verbindet die beiden Strings string1 und string2 zu einem neuen String. Es ist auch möglich, mehrere Strings gleichzeitig zu verketten, indem man sie mit einem Komma trennt:

```Elixir
string1 <> string2 <> string3
```

Hier ein einfaches Beispiel, um dies in Aktion zu sehen:

```Elixir
string1 = "Hallo"
string2 = "Welt"
puts string1 <> " " <> string2

# Ausgabe: Hallo Welt
```

Es ist auch möglich, Variablen in die Verbindung von Strings einzubeziehen, indem man diese in geschweifte Klammern setzt:

```Elixir
string1 = "Es ist"
string2 = "heute"
puts "{string1} ein {string2} schöner Tag."

# Ausgabe: Es ist ein heute schöner Tag.
```

## Deep Dive

Elixir wird immer die richtige Kodierung für die Verkettung von Strings auswählen. Dies bedeutet, dass es nicht auf die Art der verwendeten Zeichen oder die verwendete Kodierung des Systems ankommt.

Ein weiteres wichtiges Konzept beim Verbinden von Strings in Elixir ist die Verwendung von Binärdaten. Binärdaten werden in Elixir verwendet, um effizient mit Rohdaten zu arbeiten. Dadurch können Zeichen nicht automatisch in UTF-8 oder andere Kodierungen umgewandelt werden.

Um Binärdaten zu verketten, verwendet man den `<<>>`-Operator:

```Elixir
binary1 = <<65>>
binary2 = <<66>>
new_binary = binary1 <> binary2
```

Dies verbindet die beiden Binärdaten und erstellt eine neue Binärdatei, die beide enthält.

## Siehe auch
- [Elixir-Dokumentation zum Thema String-Verkettung](https://elixir-lang.org/getting-started/strings.html#string-interpolation-and-concatenation)
- [Erfahren Sie mehr über Binärdaten in Elixir](https://elixir-lang.org/getting-started/binaries-strings-and-charlists.html#binaries)
- [Ein Blog-Beitrag über die Vorteile von Binärdaten in Elixir](https://blog.drewolson.org/elixir/2019/04/15/benefits-of-binaries-in-elixir.html)