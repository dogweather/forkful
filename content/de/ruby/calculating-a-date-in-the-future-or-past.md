---
title:    "Ruby: Das Berechnen eines Datums in der Zukunft oder Vergangenheit."
keywords: ["Ruby"]
---

{{< edit_this_page >}}

## Warum
Es gibt unzählige Gründe, warum man ein Datum in der Zukunft oder Vergangenheit berechnen würde. Vielleicht planst du ein wichtiges Ereignis, möchtest eine bestimmte Zeitspanne berechnen oder einfach nur dein Wissen über Ruby vertiefen. Wie auch immer, das Berechnen von Datum kann eine nützliche Fähigkeit sein, die du in deinem Programmierrepertoire haben solltest.

## Wie das geht

Die Berechnung von Datum in Ruby ist relativ einfach und erfordert nur ein paar Schritte. Zunächst musst du das Datum angeben, von dem du ausgehen möchtest. Das kann entweder ein festes Datum sein oder das aktuelle Datum. Dann musst du entscheiden, ob du ein Datum in der Zukunft oder Vergangenheit berechnen möchtest. Schließlich musst du angeben, um wie viele Tage, Monate oder Jahre es sich handeln soll.

Um ein Datum in Ruby zu berechnen, kannst du die Methode `Date#advance` verwenden. Hier ist ein Beispiel:

```Ruby
require "date"

start_date = Date.today
# => #<DateTime: 2021-08-24T00:00:00+02:00 ((2459444j,0s,0n),+7200s,2299161j)>

future_date = start_date.advance(days: 5)
# => #<DateTime: 2021-08-29T00:00:00+02:00 ((2459449j,0s,0n),+7200s,2299161j)>

past_date = start_date.advance(months: -3)
# => #<DateTime: 2021-05-24T00:00:00+02:00 ((2459342j,0s,0n),+7200s,2299161j)>
```

Wie du sehen kannst, gibt die Methode `Date#advance` ein neues `DateTime` Objekt zurück, das das berechnete Datum enthält.

## Tiefer Einblick

Die Methode `Date#advance` akzeptiert verschiedene Argumente, einschließlich `days`, `months`, `years`, `weeks`, `hours`, `minutes` und `seconds`. Du kannst auch negative Zahlen verwenden, um ein Datum in der Vergangenheit zu berechnen. Diese Methode ist sehr flexibel und kann auf verschiedene Arten verwendet werden.

Neben der `Date#advance` Methode gibt es auch die `Date#since` Methode, die ähnliche Funktionalitäten bietet. Du kannst auch andere Formate wie `Date#in` und `Date#ago` verwenden. Es ist wichtig zu beachten, dass diese Methoden nur im `Date` Modul verfügbar sind und nicht in anderen Ruby-Modulen.

## Siehe auch
- [Ruby Dokumentation zu Datum und Zeit](https://ruby-doc.org/stdlib-2.7.0/libdoc/date/rdoc/Date.html)
- [Tutorial zu Datum und Zeit in Ruby](https://www.rubyguides.com/2019/07/ruby-date-and-time/)
- [Berechnung von Datum in anderen Programmiersprachen](https://www.journaldev.com/33226/add-subtract-days-hours-minutes-date-time-python-java-php)