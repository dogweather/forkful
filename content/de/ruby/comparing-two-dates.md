---
title:    "Ruby: Vergleich von zwei Datum"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/ruby/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Warum
Das Vergleichen von zwei Daten kann eine nützliche Fähigkeit in der Ruby Programmierung sein. Es kann dabei helfen, zu bestimmen, ob ein Datensatz in einer Datenbank älter oder neuer als ein anderer ist. In diesem Blog-Beitrag werden wir lernen, wie man diese Fähigkeit in Ruby ausführt und einige interessante Fakten über die Arbeit mit Daten herausfinden.

## Wie geht das
Es gibt mehrere Methoden, um zwei Daten in Ruby zu vergleichen. Wir werden uns hier auf die gängigsten Methoden konzentrieren.

### Verwenden von Vergleichsoperatoren
Eine Möglichkeit, zwei Daten in Ruby zu vergleichen, ist die Verwendung von Vergleichsoperatoren wie `<` (kleiner als), `>` (größer als) und `==` (gleich). Schauen wir uns ein Beispiel an:

```Ruby
date_1 = Date.new(2020, 2, 5)
date_2 = Date.new(2020, 3, 15)

if date_1 < date_2
  puts "Date 1 ist vor Date 2."
elsif date_1 > date_2
  puts "Date 1 ist nach Date 2."
else
  puts "Date 1 und Date 2 sind gleich."
end
```

Die Ausgabe dieses Codes wäre "Date 1 ist vor Date 2." Dies liegt daran, dass `date_1` ein früheres Datum ist als `date_2`. Durch die Verwendung von Vergleichsoperatoren können wir also bestimmen, welche der beiden Daten früher oder später ist.

### Verwenden von Methode #compare
Eine weitere Möglichkeit, zwei Daten in Ruby zu vergleichen, ist die Verwendung der Methode `#compare`. Diese Methode gibt entweder `0`, `1` oder `-1` zurück, abhängig davon, ob die Daten gleich, größer oder kleiner sind. Sehen wir uns dazu ein Beispiel an:

```Ruby
date_1 = Date.new(2020, 4, 20)
date_2 = Date.new(2020, 4, 15)

if date_1.compare(date_2) == 1
  puts "Date 1 ist nach Date 2."
elsif date_1.compare(date_2) == -1
  puts "Date 1 ist vor Date 2."
else
  puts "Date 1 und Date 2 sind gleich."
end
```

In diesem Fall wäre die Ausgabe "Date 1 ist nach Date 2." Da `date_1` ein späteres Datum ist als `date_2`, gibt die Methode `#compare` eine `1` zurück.

## Tiefer gehend
In diesen Code-Beispielen haben wir nur das Vergleichen von einfachen Datumswerten betrachtet. Es ist jedoch auch möglich, zwei Daten zu vergleichen, die Zeit- und Zeitzone-Informationen enthalten. Dazu ist es wichtig, die entsprechenden Methoden und Konverter zu kennen, um die Daten in ein vergleichbares Format zu bringen.

## Siehe auch
- [Ruby Date Dokumentation](https://ruby-doc.org/stdlib-2.7.1/libdoc/date/rdoc/Date.html)
- [Date-Klasse in Ruby](https://www.rubyguides.com/2015/02/ruby-date-time-tutorial/) 
- [Vergleichsoperatoren in Ruby](https://www.rubyguides.com/2018/12/ruby-comparison-operators/)