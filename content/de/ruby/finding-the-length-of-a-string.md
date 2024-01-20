---
title:                "Die Länge eines Strings ermitteln"
html_title:           "Java: Die Länge eines Strings ermitteln"
simple_title:         "Die Länge eines Strings ermitteln"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/ruby/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# Länge einer Zeichenkette in Ruby ermitteln

## Was & Warum?

Das Ermitteln der Länge einer Zeichenkette (auch als 'String' bekannt) besagt, wie viele Zeichen sie enthält. Das ist nützlich bei Textverarbeitungsaufgaben, beim Datenabgleich, bei der Validierung von Eingabemustern und mehr.

## Wie:

Mit Ruby ist das Finden der Länge einer Zeichenkette ein Kinderspiel. Hier ist ein einfaches Beispiel:

```Ruby
str = "Hallo Welt"
# Anzahl der Zeichen mit .length ermitteln
laenge = str.length
puts "Die Länge der Zeichenkette ist #{laenge}."
```

Wenn Sie diesen Code ausführen, zeigt er "Die Länge der Zeichenkette ist 10." an.

## Vertiefung:

Die Methode `.length` ist in Ruby seit dessen erster Veröffentlichung 1995 verfügbar und gibt die Anzahl der Zeichen in einer Zeichenkette zurück. Alternativ können Sie auch `.size` verwenden, welches identische Ergebnisse liefert, da beide Methoden ähnlich implementiert sind.

```Ruby
str = "Hallo Welt"
laenge = str.size 
puts "Die Länge der Zeichenkette ist #{laenge}."
```

Dieser Code gibt ebenfalls "Die Länge der Zeichenkette ist 10." aus. 

Es ist ebenso möglich, die Länge einer Zeichenkette mithilfe der Methode `.bytesize` zu ermitteln. Diese Methode gibt allerdings die Länge des Strings in Bytes zurück, nicht in Anzahl der Zeichen.

```Ruby
str = "Hallo Welt"
laenge = str.bytesize
puts "Die Länge der Zeichenkette ist #{laenge}."
```

Wird diese Methode mit einer ASCII-Zeichenkette verwendet, stimmen die Ausgaben von `.length` und `.bytesize` überein. Bei Zeichenketten mit nicht ASCII-Zeichen jedoch nicht.

## Siehe Auch:

- Ruby-Dokumentation für die String-Klasse: https://ruby-doc.org/core-2.7.3/String.html
- Vertiefender Guide für die Arbeit mit Strings in Ruby: https://www.rubyguides.com/2018/01/ruby-string-methods/.
- Unterschied zwischen `.length` und `.bytesize` in Ruby: https://stackoverflow.com/questions/8844393/difference-between-string-length-and-string-bytesize-in-ruby.