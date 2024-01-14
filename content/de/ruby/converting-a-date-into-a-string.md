---
title:                "Ruby: Ein Datum in einen String umwandeln"
simple_title:         "Ein Datum in einen String umwandeln"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/ruby/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Warum
Dateien in Ruby zu konvertieren, kann eine nützliche Fähigkeit sein, um Informationen in einem lesbareren Format darzustellen oder um Daten mit anderen Systemen zu teilen.

## How To
Eine Datumskonvertierung in Ruby kann einfach durch die Verwendung von vordefinierten Methoden des Moduls `Date` erfolgen. Im folgenden Beispiel konvertieren wir ein Datum in einen String mit dem Format "Tag, Monat Jahr".

```Ruby
date = Date.new(2021, 4, 15)
puts date.strftime("%A, %B %Y")
```

Das Ergebnis des obigen Codes ist:

```Ruby
Thursday, April 2021
```

## Deep Dive
Das Modul `Date` bietet mehrere Methoden für die Datumskonvertierung, die es uns ermöglichen, den Ausgabestring an unsere Bedürfnisse anzupassen. Dazu gehören das Hinzufügen von Uhrzeit, Zeitzone oder die Verwendung von abgekürzten Monatsnamen anstelle von vollen Namen. Es ist auch möglich, benutzerdefinierte Formate zu erstellen, indem man die entsprechenden Symbole in `strftime` verwendet.

Es ist wichtig zu beachten, dass Datum in Ruby immer mit einer Zeitzone gespeichert werden, auch wenn sie nicht explizit angegeben wird. Daher kann es gelegentlich zu unerwarteten Ergebnissen bei der Konvertierung kommen, wenn die Zeitzone nicht beachtet wird.

## Siehe auch
- [Ruby-Dokumentation: Converting dates and times](https://ruby-doc.org/stdlib-2.7.0/libdoc/date/rdoc/Date.html#method-i-strftime)
- [Tutorial: Anfängerleitfaden zum Umgang mit Datum und Zeit in Ruby](https://www.rubyguides.com/2016/09/ruby-datetime/)
- [Video: Datum und Zeit in Ruby](https://www.youtube.com/watch?v=yolK1tdFbE8)