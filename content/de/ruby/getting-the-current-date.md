---
title:                "Das aktuelle Datum abrufen"
html_title:           "Gleam: Das aktuelle Datum abrufen"
simple_title:         "Das aktuelle Datum abrufen"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/ruby/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Aktuelles Datum in Ruby bekommen

## Was & Warum?

In der Programmierung bezieht sich "aktuelles Datum" auf das Abrufen der aktuellen, realen Weltzeit und -datums bei jeder Ausführung. Warum machen wir das? Nun, um Beiträge zu zeitstempeln, Berechnungsdaten zu aktualisierten, einer Benutzerebene Relevanz zu bieten oder Daten verknüpfte Ereignisse zu erstellen.

## Wie man es macht:

In Ruby, das aktuelle Datum ist nur einen Codeabschnitt von Ihnen entfernt. 

```Ruby
require 'date'

heute = Date.today
puts heute
```

Wenn Sie diesen Code ausführen, sehen Sie ein Ausgabe, die Ihrer aktuellen Datum entspricht. Versuchen Sie es!

## Vertiefung

_Ab dem Anfang_

Für historischen Kontext, seit Version 1.9 hat Ruby ein eingebautes Modul namens "Date", das dieses Problem elegant löst. 

_Alternativen_

Es gibt noch andere Wege, das aktuelle Datum in Ruby zu erhalten. Zum Beispiel, die `Time`-klasse gibt Ihnen Datum und Zeit.

```Ruby
puts Time.now
```

_Funktionsweise_

Intern konvertiert das `Date.today`-Verfahren das Gregorianische Datum, das es von `::new!` erhält, in ein Zivil- oder Julianisches Datum, abhängig von dem aktuell `ITALY` (der Standard) oder `ENGLAND` Startdatenmodus.

## Siehe auch

Für weitere Informationen sehen Sie es sich bitte die folgenden Resourcen an:

1. [Ruby's Date Class Documentation](https://ruby-doc.org/standard-2.5.1/libdoc/date/rdoc/Date.html)
2. [Getting the current Time instance](https://ruby-doc.org/core-2.5.1/Time.html#method-c-now)
3. [About Date and DateTime Implementation (StackOverflow)](https://stackoverflow.com/questions/5937082/what-difference-between-date-and-datetime-in-ruby)