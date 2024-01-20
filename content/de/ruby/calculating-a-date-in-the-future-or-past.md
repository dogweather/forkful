---
title:                "Berechnung eines zukünftigen oder vergangenen Datums"
html_title:           "Ruby: Berechnung eines zukünftigen oder vergangenen Datums"
simple_title:         "Berechnung eines zukünftigen oder vergangenen Datums"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/ruby/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Was & Warum?

Das Berechnen eines zukünftigen oder vergangenen Datums ist eine alltägliche Programmieraufgabe, die uns dabei hilft, Planungen, Zeitverfolgungen und dergleichen durchzuführen. Wie berechnen wir ein Datum in der Zukunft oder Vergangenheit? Lassen Sie uns das in Ruby erkunden.

## So geht's:

In Ruby ist es ziemlich einfach, ein Datum in der Zukunft oder Vergangenheit zu berechnen. Wir nutzen dazu die eingebaute Klasse `Date`.

```Ruby
require 'date'

heute = Date.today
in_einer_woche = heute + 7
vor_einem_monat = heute << 1

puts "Heute ist es #{heute}"
puts "In einer Woche ist es #{in_einer_woche}"
puts "Vor einem Monat war es #{vor_einem_monat}"
```
Die Ausgabe dieses Codes könnte wie folgt aussehen:

```Ruby
Heute ist es 2022-01-01
In einer Woche ist es 2022-01-08
Vor einem Monat war es 2021-12-01
```

## Vertiefung:

Lassen Sie uns ein wenig tiefer eintauchen. In Ruby verwenden wir + und - Operatoren, um Tage hinzuzufügen oder abzuziehen. Der '<<' Operator wird verwendet, um Monate abzuziehen und der '>>' Operator, um Monate zu addieren.

Es ist zu beachten, dass diese Berechnungen immer gemäß dem Gregorianischen Kalender ausgeführt werden. Dies ist der internationale Standard für den zivilen Gebrauch und wurde 1582 eingeführt.

Es gibt Alternativen zu Ruby's eingebauter `Date` Klasse. Zum Beispiel bietet das 'active_support'-Gem, welches Teil von Ruby on Rails ist, weitere hilfreiche Methoden. Damit könnten Sie `30.days.from_now` oder `2.weeks.ago` schreiben.

## Siehe auch:

Hier sind einige hilfreiche Links, um mehr Informationen zum Date Handling in Ruby zu bekommen:

- Ruby Documentation: [Date](https://ruby-doc.org/stdlib-3.0.0/libdoc/date/rdoc/Date.html)
- Ruby Documentation: [Active Support Core Extensions](https://guides.rubyonrails.org/active_support_core_extensions.html)
- Stack Overflow: [How to do date/time addition and subtraction in Ruby?](https://stackoverflow.com/questions/1958997/how-to-do-date-time-addition-and-subtraction-in-ruby)