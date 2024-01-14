---
title:    "Ruby: Berechnung eines Datums in der Zukunft oder Vergangenheit"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/ruby/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Warum

Das Berechnen von Datum in der Zukunft oder Vergangenheit kann in der Programmierung sehr hilfreich sein. Zum Beispiel kann es verwendet werden, um die Lieferzeit für ein Paket oder das Ablaufdatum für eine Gutscheinaktion zu bestimmen.

## Wie man es macht

Um ein Datum in der Zukunft oder Vergangenheit zu berechnen, müssen wir zuerst das heutige Datum sowie die Anzahl der Tage hinzufügen oder subtrahieren, je nachdem, in welche Richtung wir berechnen wollen. In Ruby können wir diese Berechnung sehr einfach mit der Date-Klasse durchführen. Schauen wir uns ein Beispiel an:

```Ruby
require 'date'

today = Date.today
future_date = today + 7
past_date = today - 3

puts "Das heutige Datum ist #{today}"
puts "In einer Woche ist es der #{future_date}"
puts "Vor drei Tagen war es der #{past_date}"
```

Die Ausgabe sieht wie folgt aus:

```
Das heutige Datum ist 2021-04-25
In einer Woche ist es der 2021-05-02
Vor drei Tagen war es der 2021-04-22
```

Wie Sie sehen können, haben wir mithilfe der Date-Klasse das heutige Datum erhalten und anschließend 7 Tage für ein Datum in der Zukunft addiert oder 3 Tage für ein Datum in der Vergangenheit subtrahiert.

## Tiefergehende Informationen

In Ruby können wir nicht nur Tage, sondern auch Wochen, Monate und Jahre zum Datum hinzufügen oder subtrahieren. Hier sind einige weitere Beispiele:

```Ruby
# Eine Woche hinzufügen
future_date = today + 7

# Einen Monat hinzufügen
future_date = today >> 1

# Ein Jahr hinzufügen
future_date = today >> 12

# Eine Woche vorbeigehen lassen
future_date = today - 7

# Einen Monat vorbeigehen lassen
future_date = today << 1

# Ein Jahr vorbeigehen lassen
future_date = today << 12
```

Zusätzlich zur Date-Klasse können wir auch die Time-Klasse verwenden, um ein Datum in der Zukunft oder Vergangenheit zu berechnen. Dies kann nützlich sein, wenn wir auch die Uhrzeit in die Berechnung einbeziehen möchten.

## Siehe auch

- [Offizielle Ruby-Dokumentation für die Date-Klasse](https://ruby-doc.org/stdlib-2.7.1/libdoc/date/rdoc/Date.html)
- [Offizielle Ruby-Dokumentation für die Time-Klasse](https://ruby-doc.org/core-2.7.1/Time.html)
- [Tutorial zur Berechnung von Datum in Ruby von RubyGuides](https://www.rubyguides.com/2019/10/ruby-date-add/)