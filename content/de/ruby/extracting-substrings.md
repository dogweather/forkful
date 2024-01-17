---
title:                "Extrahieren von Teilstücken"
html_title:           "Ruby: Extrahieren von Teilstücken"
simple_title:         "Extrahieren von Teilstücken"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/ruby/extracting-substrings.md"
---

{{< edit_this_page >}}

## Was & Warum?
Wenn du schon eine Weile programmierst, hast du sicher schon öfter Substrings gehört. Es ist einfach ein Teil eines String, der einen Teil der ursprünglichen Zeichenkette beinhaltet. Das kann sehr hilfreich sein, um bestimmte Daten aus einem String zu extrahieren oder um Strings zu durchsuchen. Substrings sind eine grundlegende Funktion in vielen Programmiersprachen, einschließlich Ruby, und werden häufig in der Praxis verwendet.

## Wie geht's?
Substrings können auf verschiedene Arten in Ruby extrahiert werden. Die einfachste Methode ist die Verwendung von Indizes innerhalb eckigen Klammern ```[start, length]```, wobei ```start``` der Index des ersten Zeichens und ```length``` die Anzahl der zu extrahierenden Zeichen ist. Ein Beispiel dafür wäre:

```Ruby
str = "Hallo Welt!"
puts str[3, 4]

# Output: lo W
```

Du kannst auch negative Indizes verwenden, um von hinten zu zählen. Zum Beispiel:

```Ruby
str = "Hallo Welt!"
puts str[-5, 2]

# Output: We
```

Eine andere Methode ist die Verwendung von Regulären Ausdrücken, auch bekannt als Regex. Mit Regex kannst du nach bestimmten Mustern in einem String suchen und Substrings basierend auf diesen Mustern extrahieren. Zum Beispiel:

```Ruby
str = "Hallo Welt!"
puts str[/e.*t/]

# Output: elt
```

Weitere nützliche Methoden zum Extrahieren von Substrings sind ```slice```, ```slice!```, ```substring``` und ```split```. Sie alle bieten verschiedene Möglichkeiten, um Substrings zu extrahieren und zu manipulieren.

## Tiefgehende Einblicke
Substrings haben ihren Ursprung in der Informatik und sind eine grundlegende Funktion von Programmiersprachen. Sie wurden entwickelt, um Datenverarbeitungsaufgaben wie Textverarbeitung und Suchalgorithmen zu erleichtern. Eine der Alternativen zu Substrings ist die Verwendung von regulären Ausdrücken, wie bereits erwähnt. Diese können jedoch komplexer und schwieriger zu lesen und zu verstehen sein.

Die Implementierung von Substrings in Ruby basiert auf der Ruby-Bibliothek String, die verschiedene Methoden zur Manipulation von Zeichenketten bietet. Ruby ermöglicht es auch, benutzerdefinierte Methoden für Strings zu erstellen, um Substrings auf spezifischere Weise zu extrahieren.

## Siehe auch
- [Ruby String Documentation](https://ruby-doc.org/core-3.0.1/String.html)
- [Regular Expressions in Ruby](https://www.regular-expressions.info/ruby.html)
- [Ruby String Manipulation Tutorial](https://www.rubyguides.com/2018/02/ruby-string-methods/)