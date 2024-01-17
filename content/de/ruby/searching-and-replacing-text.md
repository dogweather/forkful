---
title:                "Suchen und Ersetzen von Text"
html_title:           "Ruby: Suchen und Ersetzen von Text"
simple_title:         "Suchen und Ersetzen von Text"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/ruby/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Was & Warum?
Die Suche und Ersetzung von Text ist ein häufig verwendetes Konzept in der Programmierung. Es bezieht sich auf das Finden von bestimmten Zeichenketten in einem Text und das Ersetzen durch andere Zeichenketten. Dies wird oft verwendet, um Fehler zu korrigieren oder Text auf bestimmte Weise zu formatieren.

## So geht's:
Um Text in Ruby zu suchen und zu ersetzen, verwenden wir die Methode `gsub`. Hier ist ein Beispiel:

```Ruby
text = "Hallo Welt"
text.gsub("Hallo", "Guten Tag") #=> "Guten Tag Welt"
```

In diesem Beispiel wird die Zeichenkette "Hallo" durch "Guten Tag" ersetzt, was zu "Guten Tag Welt" führt. Beachten Sie, dass die ursprüngliche Zeichenkette "Hallo Welt" unverändert bleibt.

## Deep Dive:
Die Idee der Suche und Ersetzung von Text ist nicht neu und wird seit Jahrzehnten in der Programmierung verwendet. Es gibt auch Alternativen zur `gsub`-Methode, wie zum Beispiel die Verwendung von regulären Ausdrücken oder das Einlesen und Schreiben von Dateien. Die Implementierung von `gsub` erfolgt auf einer niedrigen Ebene und verwendet effiziente Algorithmen, um Text zu suchen und zu ersetzen.

## Weitere Informationen:
- [Ruby Dokumentation über gsub](https://ruby-doc.org/core-2.7.1/String.html#method-i-gsub)
- [Einführung in reguläre Ausdrücke in Ruby](https://www.rubyguides.com/2015/06/ruby-regex/)
- [Ruby Dateiverarbeitung](https://www.rubyguides.com/2015/05/working-with-files-ruby/)