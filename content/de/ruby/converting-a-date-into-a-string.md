---
title:                "Ein Datum in einen String umwandeln"
html_title:           "Java: Ein Datum in einen String umwandeln"
simple_title:         "Ein Datum in einen String umwandeln"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/ruby/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?
Eine Datums-Umwandlung in einen String (Text) ermöglicht es Programmierern, das Datum in einem menschenlesbaren Format anzuzeigen oder zu speichern. Dies ist nützlich für Protokollierungs-, Anzeige- oder Kommunikationszwecke.

## Wie:
In Ruby ist das Ganze ziemlich einfach. Hier ist ein kurzes Codebeispiel:
```Ruby
require 'date'

datum = Date.today
puts datum.to_s
```
Erzeugt eine Ausgabe ähnlich wie:
```Ruby
"2023-04-15"
```

## Tief Tauchen:
Im historischen Kontext wurde diese Methode von der Klasse `Date` in Ruby eingeführt, um die Verwendung von Datumsobjekten zu erleichtern.

Es gibt alternative Wege, um das gleiche Ziel zu erreichen. Ein Beispiel ist die Formatierung des Strings mit `strftime`, die eine anpassbare Formatierung bietet.
```Ruby
datum.strftime("%d-%m-%Y") #"15-04-2023"
```
Die Implementierungsdetails der `to_s` Methode in Ruby sind ziemlich einfach. Sie konvertiert das Datumsobjekt durch Standardisierung auf das Format 'YYYY-MM-DD'. 

## Siehe auch:
Hier sind einige Ressourcen, die Ihnen weitere Hilfe und Einblicke bieten können:

1. [Date-Klasse in Ruby-Dokumenten](https://ruby-doc.org/stdlib-2.7.1/libdoc/date/rdoc/Date.html)
2. [Ruby-Datumsformatierung](https://www.foragoodstrftime.com/)
3. [Stringformatierung in Ruby](https://ruby-doc.org/core-2.4.2/String.html#method-i-format)