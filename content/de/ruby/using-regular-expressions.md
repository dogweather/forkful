---
title:                "Verwendung von regulären Ausdrücken"
html_title:           "Ruby: Verwendung von regulären Ausdrücken"
simple_title:         "Verwendung von regulären Ausdrücken"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/ruby/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Warum

Warum solltest du Regular Expressions in deinem Ruby Code verwenden? Es gibt viele Gründe, aber hier sind nur zwei: Erstens ermöglichen sie es dir, komplexe Muster in Strings zu erkennen und zu manipulieren. Zweitens sparen sie Zeit und sorgen für effizienteres Code-Schreiben, da du nicht händisch durch Strings iterieren musst.

## Wie geht das?

Es gibt drei verschiedene Wege, um Regular Expressions in Ruby zu nutzen:

1. Der `=~` Operator: Dieser kann verwendet werden, um herauszufinden, ob ein String ein bestimmtes Muster enthält. Zum Beispiel: `puts "Hello world" =~ /world/` gibt `6` zurück, da das Muster `/world/` ab der 6. Position im String gefunden wird.

```Ruby
puts "Hello world" =~ /world/  # Output: 6
```

2. Der `match()` Methode: Diese Methode kann verwendet werden, um ein Match-Objekt zurückzugeben, das Informationen über das gefundene Muster enthält. Zum Beispiel: `puts "Hello world".match(/world/)[0]` gibt `"world"` zurück.

```Ruby
puts "Hello world".match(/world/)[0]  # Output: world
```

3. Der `scan()` Methode: Diese Methode kann verwendet werden, um alle Vorkommnisse eines Musters in einem String zu finden und als Array zurückzugeben. Zum Beispiel: `puts "Hello world".scan(/l/)` gibt `["l", "l", "l"]` zurück, da das Muster `/l/` dreimal im String gefunden wird.

```Ruby
puts "Hello world".scan(/l/)  # Output: ["l", "l", "l"]
```

## Tiefergehende Einblicke

Du kannst Regular Expressions mit verschiedenen "Flag"-Optionen versehen, um dein Suchmuster anzupassen. Zum Beispiel `/string/i` sucht nach dem String "string" ohne auf Groß- und Kleinschreibung zu achten.

Es gibt auch viele weitere spezielle Zeichen, die verwendet werden können, um komplexe Muster zu erstellen, z.B.:

- `?` um ein einzelnes Zeichen optional zu machen
- `+` um das vorherige Zeichen oder Muster eins oder mehrmals zu wiederholen
- `*` um das vorherige Zeichen oder Muster null oder mehrmals zu wiederholen

Es gibt unzählige Möglichkeiten, Regular Expressions an deine Bedürfnisse anzupassen. Experimentiere damit und sie werden dir zu einem leistungsstarken Werkzeug beim Coden werden!

## Siehe auch

Weitere Informationen zu Regular Expressions in Ruby findest du hier:

- [Ruby Regular Expressions: A Comprehensive Guide](https://www.rexegg.com/regex-ruby.html)
- [Ruby Docs: Regular Expressions](https://ruby-doc.org/core-2.7.2/Regexp.html)