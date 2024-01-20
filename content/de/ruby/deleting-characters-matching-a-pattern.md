---
title:                "Zeichen löschen, die einem Muster entsprechen"
html_title:           "C#: Zeichen löschen, die einem Muster entsprechen"
simple_title:         "Zeichen löschen, die einem Muster entsprechen"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/ruby/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Was & Warum?

Das Löschen von Zeichen, die einem Muster entsprechen, bezeichnet das Entfernen spezifischer Zeichen aus einer Textsequenz basierend auf einem bestimmten Muster. Programmierer tun dies, um unerwünschte Zeichen zu entfernen oder die Textdaten zu bereinigen.

## So geht's:

Hier ein einfacher Code, wie man in Ruby Zeichen aus einem String entfernt, die einem Muster entsprechen.

```Ruby
str = "Hallo, Welt!"
str.delete!(",!")
puts str
```

Ausgabe:

```Ruby
Hallo Welt
```

In diesem Beispiel werden die Zeichen `,` und `!` aus dem String entfernt.

## Vertiefung

In der Geschichte von Ruby können `gsub` und `tr` als Alternativen zur `delete`-Methode angesehen werden. Während `gsub` den String ersetzt, wird `tr` hauptsächlich zum Ersetzen von Zeichenfolgen verwendet. `delete`,` gsub` und `tr` sind alle eingebaute Funktionen und wirken sich nicht auf die Leistungsfähigkeit des Codes aus.

Hier ist ein Beispiel dafür, wie `gsub` zum Entfernen von Zeichen verwendet werden kann:

```Ruby
str = "Hallo, Welt!"
str.gsub!(/[,!]/, '')
puts str
```

Ausgabe:

```Ruby
Hallo Welt
```

## Siehe auch

Für mehr Informationen, siehe die Ruby-Dokumentation:

- [delete](https://ruby-doc.org/core-2.7.1/String.html#method-i-delete)
- [gsub](https://ruby-doc.org/core-2.7.1/String.html#method-i-gsub)
- [tr](https://ruby-doc.org/core-2.7.1/String.html#method-i-tr)