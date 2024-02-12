---
title:                "Anführungszeichen aus einem String entfernen"
aliases:
- /de/ruby/removing-quotes-from-a-string.md
date:                  2024-01-26T03:41:21.535715-07:00
model:                 gpt-4-0125-preview
simple_title:         "Anführungszeichen aus einem String entfernen"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/ruby/removing-quotes-from-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Entfernen von Anführungszeichen aus einem String bedeutet, diese doppelten oder einfachen Anführungszeichen wegzunehmen, die Textwerte umschließen. Programmierer tun dies oft, um Benutzereingaben zu säubern, um Konsistenz in der Datenverarbeitung zu gewährleisten oder um Daten für Systeme vorzubereiten, die durch diese zusätzlichen Zeichen verwirrt werden könnten.

## Wie:
Ruby hat einige nette Tricks auf Lager, um diese lästigen Anführungszeichen herauszuschneiden. Du kannst die Methoden `gsub` oder `delete` verwenden, um die Aufgabe zu erledigen. Hier ist etwas Code zum Nachdenken:

```ruby
# Verwendung von gsub, um doppelte und einfache Anführungszeichen zu entfernen
quoted_string = "\"Sag 'Hallo' zu meinem kleinen Freund!\""
unquoted_string = quoted_string.gsub(/'|"/, '')
puts unquoted_string 
# Ausgabe: Sag Hallo zu meinem kleinen Freund!

# Wenn du weißt, dass du nur mit einer Art von Anführungszeichen umgehen wirst
single_quoted_string = "'Bleib eine Weile und hör mir zu!'"
clean_string = single_quoted_string.delete("'")
puts clean_string 
# Ausgabe: Bleib eine Weile und hör mir zu!
```

## Tiefer eintauchen
Die Geschichte der Anführungszeichen reicht zurück bis zu den frühesten Tagen der Programmierung, wo sie oft als Zeichenkettenbegrenzer dienten. Heutzutage, wie damals, findest du dich vielleicht in der Situation, dass du diese Anführungszeichen entfernen musst, wenn sie nicht benötigt werden oder wenn sie die Datenspeicherung und -manipulation stören könnten.

Wir haben über `gsub` und `delete` gesprochen, aber es gibt auch andere Methoden, wie `tr` oder `tr_s`, die dir etwas mehr Kontrolle geben oder einige unterschiedliche Anwendungsfälle bewältigen können:

```ruby
# tr kann ebenfalls Anführungszeichen entfernen
double_quoted_string = "\"Tun oder nicht tun, es gibt kein Versuchen.\""
clean_string = double_quoted_string.tr('\"', '')
puts clean_string 
# Ausgabe: Tun oder nicht tun, es gibt kein Versuchen.
```

Denk daran, jede dieser Methoden hat ihre Anwendungsfälle. `gsub` ist mächtiger, wenn du mit komplexen Mustern oder mehrfachen Ersetzungen zu tun hast. `delete` und `tr` funktionieren wunderbar für einfache, unkomplizierte Zeichenentfernungen.

## Siehe auch
Für weiterführende Literatur und um diese Methoden in Aktion innerhalb größerer Codebasen zu sehen, schau dir an:
- Die Ruby-Dokumentation für [String#gsub](https://ruby-doc.org/core-3.1.2/String.html#method-i-gsub), [String#delete](https://ruby-doc.org/core-3.1.2/String.html#method-i-delete) und [String#tr](https://ruby-doc.org/core-3.1.2/String.html#method-i-tr).
- Ruby Monstas hat einen großartigen [String Exercise Set](http://ruby-for-beginners.rubymonstas.org/built_in_classes/strings.html), das auch das Arbeiten mit Anführungszeichen umfasst.
- Diskussionen auf Stack Overflow über [Stringmanipulation](https://stackoverflow.com/search?q=ruby+remove+quotes+from+string) bieten Probleme und Lösungen aus der realen Welt von anderen Rubyisten.
