---
title:                "Befehlszeilenargumente lesen"
html_title:           "Arduino: Befehlszeilenargumente lesen"
simple_title:         "Befehlszeilenargumente lesen"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/ruby/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Was & Warum?

Eingabeaufforderungsargumente (command line arguments) in Ruby sind die Werte, die nach dem Skriptnamen beim Ausführen in Ihrer Konsole angegeben werden. Sie sind wichtig, weil sie es Programmierern ermöglichen, flexibel Daten an ihre Skripte zur Laufzeit zu übergeben.

## Wie es geht:

In Ruby greifen wir auf Eingabeaufforderungsargumente zu, indem wir das globale Array `ARGV` verwenden. Hier ist ein einfacher Codeausschnitt:

```Ruby
# argtest.rb
puts "Du hast #{ARGV.length} Argumente angegeben."
ARGV.each do |arg|
  puts "Argument: #{arg}"
end
```

Führe dieses Skript in deiner Konsole aus und gib einige Argumente ein:

```bash
ruby argtest.rb eins zwei drei
```

Erwartete Ausgabe:

```bash
Du hast 3 Argumente angegeben.
Argument: eins
Argument: zwei
Argument: drei
```

## Tiefere Informationen:

1. Historischer Kontext: Der Zugriff auf Eingabeaufforderungsargumente in Ruby durch das globale Array `ARGV` stammt aus älteren Programmiersprachen wie C und Perl, die ähnliche Konstruktionen benutzen.

2. Alternativen: Für komplexere Anwendungsfälle gibt es Bibliotheken wie `OptionParser`, die Robustheit und erweiterte Funktionen zum Parsen von Eingabeaufforderungsargumenten bieten.

3. Umsetzungsdetails: `ARGV` ist ein Array, das alle Argumente als Strings speichert. Es sollte beachtet werden, dass Argumente nicht automatisch in andere Datentypen umgewandelt werden, und es obliegt dem Programmierer, dies manuell zu tun.

## Siehe auch:


2. [Ruby Dokumentation zu OptionParser](https://ruby-doc.org/stdlib-2.7.0/libdoc/optparse/rdoc/OptionParser.html)