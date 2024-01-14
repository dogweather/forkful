---
title:    "Ruby: Umwandeln eines Strings in Kleinbuchstaben"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

## Warum

Man könnte sich fragen, warum es überhaupt notwendig ist, einen String in Kleinbuchstaben (lower case) umzuwandeln. Einer der Gründe dafür könnte sein, dass man sicherstellen möchte, dass alle Buchstaben in einem Wort einheitlich groß oder klein geschrieben sind, um beispielsweise Vergleiche oder Suchen zu erleichtern.

## Wie man es macht

Es gibt verschiedene Möglichkeiten, einen String in Ruby in Kleinbuchstaben umzuwandeln. Die einfachste Methode ist die Verwendung der Methode `downcase`:

```Ruby
x = "HELLO WORLD"
puts x.downcase
```

Output: `hello world`

Man kann auch die Methode `capitalize` verwenden, um den ersten Buchstaben eines Strings in einen Kleinbuchstaben umzuwandeln:

```Ruby
x = "hello world"
puts x.capitalize
```

Output: `Hello world`

Eine weitere Möglichkeit ist die Verwendung der Methode `swapcase`, mit der man zwischen Groß- und Kleinbuchstaben wechseln kann:

```Ruby
x = "hElLo WoRlD"
puts x.swapcase
```

Output: `HeLlO wOrLd`

## Tiefere Einblicke

Die Methode `downcase` kann auch auf mehrere Wörter gleichzeitig angewendet werden, indem man sie auf einen ganzen String anwendet:

```Ruby
x = "HELLO WORLD"
puts x.downcase
```

Output: `hello world`

Außerdem ist es wichtig zu beachten, dass die Methode `downcase` keine Akzente oder Sonderzeichen umwandeln kann. Um dies zu erreichen, kann man die Anwendung der `unicode_normalize` Methode in Kombination mit `downcase` verwenden:

```Ruby
x = "ÉLÈVE"
puts x.unicode_normalize(:nfd).downcase
```

Output: `élève`

## Siehe auch

- [Strings in Ruby](https://www.tutorialspoint.com/ruby/ruby_strings.htm)
- [String Methoden in Ruby](https://ruby-doc.org/core-2.7.2/String.html)