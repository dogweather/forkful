---
title:                "Strings verketten"
html_title:           "Bash: Strings verketten"
simple_title:         "Strings verketten"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/ruby/concatenating-strings.md"
---

{{< edit_this_page >}}

## Was & Warum?

Das Verketten von Strings erlaubt es uns, zwei oder mehrere Strings zu einem zusammenzuführen. Als Programmierer tun wir das häufig, um dynamischen Text zu erzeugen oder um Informationen für die Nutzer aufzubereiten.

## Wie:

String-Verkettung in Ruby ist einfach. Hier sind einige Beispiele: 

```Ruby
# Mit dem "+" Operator:
hallo = "Hallo,"
welt = " Welt!"
nachricht = hallo + welt
puts nachricht  # Ausgabe: Hallo, Welt!

# Mit "<<", dem "append" Operator:
nachricht = "Hallo,"
nachricht << " Welt!"
puts nachricht  # Ausgabe: Hallo, Welt!

# Mit der "concat"-Methode:
nachricht = "Hallo,".concat(" Welt!")
puts nachricht  # Ausgabe: Hallo, Welt!
```

## Tief Tauchen:

In Ruby wurde das Verketten von Strings von Anfang an unterstützt und wurde seitdem optimiert. Alternativ zu den oben genannten Methoden könnten wir auch den `#{}`-Mechanismus nutzen, um Variablen in Strings einzubetten:

```Ruby
# Mit "#{}":
welt = "Welt"
nachricht = "Hallo, #{welt}!"
puts nachricht  # Ausgabe: Hallo, Welt!
```

Im Gegensatz zu den anderen Operatoren erzeugt `+` immer einen neuen String, während `<<` und `concat` den ursprünglichen String ändern. Das ist von Bedeutung, wenn es auf die Performanz oder Speichernutzung ankommt. 

## Siehe Auch:

Weitere Informationen zu String-Verkettung und der Sprache können unter den folgenden Links gefunden werden:

1. [String Concatenation (Ruby-Dokumentation)](https://ruby-doc.org/core-2.7.0/String.html#method-i-2B-40).
3. [Stack Overflow: Difference between << and + for concatenating strings in Ruby](https://stackoverflow.com/questions/4684446/why-is-the-shovel-operator-preferred-over-plus-for-concatenating-ruby).