---
title:                "Eine Zeichenfolge interpolieren"
html_title:           "Ruby: Eine Zeichenfolge interpolieren"
simple_title:         "Eine Zeichenfolge interpolieren"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/ruby/interpolating-a-string.md"
---

{{< edit_this_page >}}

# Was & Warum?
Interpolieren ist das Einfügen von Variablen oder Ausdrücken in eine Zeichenkette, um die Ausgabe dynamisch zu gestalten. Programmierer verwenden dies, um Texte zu personalisieren oder komplexe String-Ausdrücke zu erstellen.

# Wie geht's?
```
Ruby string = "Hello, #{name}! Today is #{Date.today}."
puts string
```
Ausgabe: "Hallo, [Name]! Heute ist [aktuelles Datum]."

In diesem Beispiel wird der Name und das aktuelle Datum in die Zeichenkette eingefügt. Dabei wird die Syntax `#{...}` verwendet, um die Variable oder den Ausdruck innerhalb der Zeichenkette aufzurufen.

# Tiefere Einblicke
Interpolieren wurde in Ruby eingeführt, um das Zusammenfügen von Strings und Variablen zu vereinfachen. Eine Alternative hierzu wäre das Verwenden von `+` oder der `concat`-Methode, um die Strings manuell zu verbinden.

Interpolierung ist auch bei der Verarbeitung von regulären Ausdrücken nützlich, da sie es ermöglicht, Variablen innerhalb von Mustern zu verwenden.

Die Implementierung der String-Interpolierung in Ruby basiert auf dem Konzept der Templates, welche eine dynamische Erstellung von Texten ermöglichen. Dabei werden die Inhalte dieser Templates während der Laufzeit mit Variablen oder Ausdrücken ergänzt.

# Siehe auch
Weitere Informationen zur String-Interpolierung in Ruby finden Sie auf der offiziellen Ruby-Dokumentationsseite unter https://ruby-doc.org/core-3.0.0/doc/syntax/literals_rdoc.html#label-Here+Documents.