---
title:                "Löschen von Zeichen, die einem Muster entsprechen"
aliases: - /de/ruby/deleting-characters-matching-a-pattern.md
date:                  2024-01-20T17:43:02.508790-07:00
model:                 gpt-4-1106-preview
simple_title:         "Löschen von Zeichen, die einem Muster entsprechen"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/ruby/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Was & Warum?
Mit Ruby kannst du leicht Zeichen, die einem bestimmten Muster entsprechen, aus einem String entfernen. Wir machen das, um unerwünschte Daten zu säubern oder um die Eingabe zu formatieren.

## So geht's:
```Ruby
# Beispiel: Entferne alle Ziffern aus einem String
string = "Ruby 2.7.0 ist am 25. Dezember 2019 erschienen"
clean_string = string.gsub(/[0-9]/, '')
puts clean_string
# Ausgabe: Ruby . ist am . Dezember erschienen

# Beispiel: Entferne alle nicht-alphanumerischen Zeichen
string2 = "Hallo Welt! %$&/()=?;:"
alpha_numeric_string = string2.gsub(/[^a-zA-Z0-9\s]/, '')
puts alpha_numeric_string
# Ausgabe: Hallo Welt
```

## Tiefgang
Die Methode `gsub` in Ruby ermöglicht das globale Ersetzen von Mustern in Strings und ist seit den Anfängen der Sprache verfügbar. Alternativen dazu sind `sub`, was nur das erste Vorkommen ersetzt, oder `delete`/`delete!`, die spezifische Zeichen ohne Musterabgleich entfernen. Die `gsub`-Methode funktioniert mit regulären Ausdrücken, die eine mächtige Möglichkeit bieten, Muster zu definieren. So kannst du ziemlich komplexe Textmanipulationen mit nur einer Zeile Code durchführen.

## Siehe auch
- [Ruby-Dokumentation für `gsub`](https://ruby-doc.org/core-2.7.0/String.html#method-i-gsub)
- [Tutorial zu regulären Ausdrücken in Ruby](https://www.rubyguides.com/2015/06/ruby-regex/)
- [Ruby-Dokumentation für `delete`](https://ruby-doc.org/core-2.7.0/String.html#method-i-delete)
