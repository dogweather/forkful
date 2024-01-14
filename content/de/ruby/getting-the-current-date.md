---
title:                "Ruby: Das Abrufen des aktuellen Datums"
simple_title:         "Das Abrufen des aktuellen Datums"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/ruby/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Warum

Die aktuelle Datum und Uhrzeit spielen eine wichtige Rolle in der Programmierung, sei es für die Erstellung von Zeitstempeln oder für die Berechnung von Ablaufdaten. Mit Ruby ist es ganz einfach, das aktuelle Datum und die Uhrzeit zu erhalten. Lesen Sie weiter, um zu erfahren, wie Sie dies in Ihrer Ruby-Codebasis umsetzen können.

## Wie Geht's

Um das aktuelle Datum zu erhalten, können Sie die Ruby Methode `Time.now` verwenden. Diese Methode gibt ein Objekt des Typs `Time` zurück, das das aktuelle Datum und die aktuelle Uhrzeit repräsentiert. Hier ist ein Beispielcode:

```
```Ruby
# Erhalte das aktuelle Datum und die Uhrzeit
current_time = Time.now 

# Gib das Datum und die Uhrzeit im Standardformat aus
puts current_time 
```
Ausgabe:
`2021-06-15 14:30:00 +0300`

Sie können auch bestimmte Teile des Datums oder der Uhrzeit extrahieren, indem Sie die entsprechenden Methoden anwenden. Hier sind einige Beispiele:

```
```Ruby
current_time = Time.now 

# Erhalte den Tag im Monat
puts current_time.day 
# Ausgabe: 15

# Erhalte den Monat im Jahr
puts current_time.month 
# Ausgabe: 06

# Erhalte die aktuelle Uhrzeit im 24-Stunden-Format
puts current_time.hour 
# Ausgabe: 14
```

## Tief Tauchen

Das `Time` Objekt verfügt über viele hilfreiche Methoden für die Arbeit mit Datum und Uhrzeit. Sie können z.B. zwei `Time` Objekte miteinander vergleichen, um die Differenz zwischen ihnen zu berechnen. Hier ist ein Beispiel:

```
```Ruby
# Erstelle zwei Zeit-Objekte
time1 = Time.new(2021, 5, 10, 12, 00, 00)
time2 = Time.new(2021, 6, 15, 14, 30, 00)

# Berechne die Differenz in Sekunden
difference = time2 - time1 

# Konvertiere die Differenz in Tagen
days = difference / (24 * 60 * 60)

# Gib die Differenz in Tagen aus
puts days 
# Ausgabe: 36.0625
```

Sie können auch das aktuelle Datum und die Uhrzeit in einem bestimmten Format ausgeben, indem Sie die Methode `strftime` verwenden. Diese Methode nimmt ein String-Argument entgegen, das das gewünschte Format definiert. Hier sind einige Beispiele:

```
```Ruby
# Erhalte das aktuelle Datum und die Uhrzeit
current_time = Time.now 

# Gib das aktuelle Datum, die Uhrzeit und den Zeitzone im Format Tag Monat Jahr / Stunde:Minute aus
puts current_time.strftime("%d %B %Y / %H:%M")
# Ausgabe: 15 June 2021 / 14:30

# Gib das aktuelle Datum und die Uhrzeit im Format Monat/Jahr aus
puts current_time.strftime("%m/%y")
# Ausgabe: 06/21

# Gib das aktuelle Datum und die Uhrzeit im Unix-Zeitstempel-Format aus
puts current_time.strftime("%s")
# Ausgabe: 1623760200
```

## Siehe Auch

- [Ruby Time-Dokumentation] (https://ruby-doc.org/core-2.7.2/Time.html)
- [Ruby strftime Methodendokumentation] (https://ruby-doc.org/core-2.7.2/Time.html#method-i-strftime)

Mit diesen Informationen sollten Sie nun in der Lage sein, das aktuelle Datum und die Uhrzeit in Ihrem Ruby-Code zu verwenden. Viel Spaß beim Programmieren!