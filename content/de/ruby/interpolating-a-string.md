---
title:                "Eine Zeichenkette interpolieren"
html_title:           "Arduino: Eine Zeichenkette interpolieren"
simple_title:         "Eine Zeichenkette interpolieren"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/ruby/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?
String-Interpolation ermöglicht es, einem String Variablen oder Ausdrücke hinzuzufügen, die automatisch umgewandelt und in den String eingefügt werden. Programmierer machen dies, um Strings nach Bedarf dynamisch zu erstellen.

## Wie macht man das?
Hier ist ein einfaches Beispiel:

```Ruby
name = "Hans"
puts "Hallo, #{name}!"
```

Ausgabe:

```
Hallo, Hans!
```

Der Ausdruck innerhalb der geschweiften Klammern `{}` wird umgewandelt und an diese Stelle im String eingefügt.

## Tiefgang
Die String-Interpolation in Ruby ist seit der ersten Veröffentlichung enthalten und wurde aus Perl übernommen. 

Alternativ kann die Funktion `sprintf` oder der `%`-Operator für die Formatierung verwendet werden, diese sind aber weniger benutzerfreundlich und intuitiv als die String-Interpolation.

Hier ist ein Beispiel mit `sprintf`:

```Ruby
name = "Hans"
puts sprintf("Hallo, %s!", name)
```

Und ein Beispiel mit dem `%`-Operator:

```Ruby
name = "Hans"
puts "Hallo, %s!" % name
```

Bei der String-Interpolation wird der innerhalb der `{}`-Klammern liegende Ausdruck von Ruby's `to_s` Methode in einen String umgewandelt.

## Siehe auch
- Offizielle Ruby-Dokumentation zur String-Interpolation: [Link](https://ruby-doc.org/core-2.7.0/String.html#method-i-25-3C)
- Einführung in die String-Interpolation in Ruby: [Link](https://www.rubyguides.com/2018/11/ruby-string-interpolation/)
- Unterschied zwischen `sprintf`, `%`-Operator und String-Interpolation: [Link](https://stackoverflow.com/questions/610839/how-do-i-use-string-formatting-in-ruby)