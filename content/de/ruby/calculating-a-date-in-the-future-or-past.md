---
title:                "Berechnen eines Datums in der Zukunft oder Vergangenheit"
html_title:           "Ruby: Berechnen eines Datums in der Zukunft oder Vergangenheit"
simple_title:         "Berechnen eines Datums in der Zukunft oder Vergangenheit"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/ruby/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Warum

Du hast dich vielleicht schon einmal gefragt, wie man in Ruby ein Datum in der Zukunft oder Vergangenheit berechnen kann. Es gibt viele Gründe, dies zu tun, wie zum Beispiel die Erstellung von Planungs-Tools, die Berechnung von Fristen oder das Hinzufügen von Datumsfunktionen zu deinen Programmen.

## Wie geht man vor

Um ein Datum in der Zukunft oder Vergangenheit zu berechnen, gibt es mehrere Schritte, die du befolgen musst:

1. Importiere das `Date`-Modul in dein Ruby-Programm.
2. Verwende die Methode `today` des `Date`-Moduls, um das aktuelle Datum zu erhalten.
3. Verwende die Methode `+` oder `-` des `Date`-Objekts, um das gewünschte Datum in der Zukunft oder Vergangenheit zu berechnen.

Schau dir diese Beispiele an:

```Ruby
# Heutiges Datum abrufen
today = Date.today
# Ein Datum in der Zukunft berechnen (1 Woche später)
future_date = today + 7
# Ein Datum in der Vergangenheit berechnen (1 Woche vorher)
past_date = today - 7

# Ausgabe des Ergebnisses
puts "Heute: #{today}"
puts "Zukünftiges Datum: #{future_date}"
puts "Vergangenes Datum: #{past_date}"
```

Das obige Programm wird folgende Ausgabe erzeugen:

```
Heute: 2021-08-08
Zukünftiges Datum: 2021-08-15
Vergangenes Datum: 2021-08-01
```

## Tiefere Einblicke

Wenn du dich für weitere Details über die Berechnung von Datum in Ruby interessierst, hier sind ein paar zusätzliche Informationen:

- Du kannst verschiedene Einheiten wie Tage, Wochen, Monate und Jahre verwenden, um ein Datum zu verändern. Zum Beispiel `today + 3` fügt 3 Tage zu `today` hinzu, während `today + 1.month` ein Monat zum Datum hinzufügt.
- Du kannst auch Datums-Objekte miteinander vergleichen, um zu sehen, welches Datum später ist. Du kannst auch überprüfen, ob ein Datum vor oder nach einem bestimmten Datum liegt.
- Du kannst das `strftime`-Methode verwenden, um das Datum in einem bestimmten Format auszugeben. Zum Beispiel `today.strftime("%d.%m.%Y")` gibt das Datum im Format "Tag.Monat.Jahr" aus.

## Sieh auch

Hier sind ein paar nützliche Links, die dir bei der Berechnung von Datum in Ruby helfen können:

- [Ruby Date Dokumentation](https://ruby-doc.org/stdlib-2.7.2/libdoc/date/rdoc/Date.html)
- [Ruby Date und Time Tutorial](https://www.tutorialspoint.com/ruby/ruby_date_time.htm)
- [Praktische Ruby Methoden zur Datummanipulation](https://www.rubyguides.com/2015/09/ruby-time/)