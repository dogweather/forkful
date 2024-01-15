---
title:                "Das aktuelle Datum erhalten."
html_title:           "Ruby: Das aktuelle Datum erhalten."
simple_title:         "Das aktuelle Datum erhalten."
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/ruby/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Warum

Das Abrufen des aktuellen Datums kann in vielen Anwendungen nützlich sein, z.B. für die Anzeige der aktuellen Zeit oder für die Datumsberechnung in einer Aufgabenverwaltung.

## Wie man das aktuelle Datum in Ruby erhält

Um das aktuelle Datum in Ruby zu erhalten, nutzen wir die `Date`-Klasse und rufen ihre `today`-Methode auf:

```Ruby
heute = Date.today
puts heute
```

Die Ausgabe ist das heutige Datum im Format `YYYY-MM-DD`, z.B. `2021-05-05`.

## Tiefergehende Information

Die `Date`-Klasse gehört zum Standardpaket von Ruby und bietet viele nützliche Methoden für die Arbeit mit Datumsangaben. Wenn wir der `today`-Methode ein Argument übergeben, können wir das aktuelle Datum in einem anderen Format erhalten, z.B. als String mit `DD.MM.YYYY`:

```Ruby
heute = Date.today.strftime("%d.%m.%Y")
puts heute
```

Die Ausgabe wäre dann z.B. `05.05.2021`.

Eine weitere hilfreiche Methode ist `strftime`, mit der wir ein `DateTime`-Objekt in ein gewünschtes Format umwandeln können. Hier ein Beispiel für die Ausgabe im 12-Stunden-Format:

```Ruby
jetzt = DateTime.now.strftime("%I:%M%p")
puts jetzt
```

Die Ausgabe wäre dann z.B. `05:50PM`.

## Siehe auch

- [Ruby-Dokumentation zu Dateien](https://www.ruby-doc.org/stdlib-2.5.3/libdoc/date/rdoc/Date.html)
- [Rails-Dokumentation zu Datumsformaten](https://guides.rubyonrails.org/active_support_core_extensions.html#time-date-and-active-support)
- [Stack Overflow-Thread zu Datumsformaten in Ruby](https://stackoverflow.com/questions/25200598/converting-date-format-in-ruby)