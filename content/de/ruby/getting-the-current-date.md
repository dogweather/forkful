---
title:    "Ruby: Das aktuelle Datum erhalten"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/ruby/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Warum
Ruby ist eine leistungsstarke Programmiersprache, die von vielen Entwicklern auf der ganzen Welt geschätzt wird. Eines der nützlichsten Features von Ruby ist die Fähigkeit, das aktuelle Datum und Uhrzeit abzurufen. In diesem Blogbeitrag werden wir uns anschauen, wie man in Ruby das aktuelle Datum abrufen kann und warum dies wichtig ist.

## Wie man das aktuelle Datum abruft
Um das aktuelle Datum und die Uhrzeit in Ruby abzurufen, gibt es verschiedene Methoden. Die einfachste ist die Verwendung der `Time`-Klasse. Hier ist ein Beispiel:

```Ruby
current_date = Time.now
puts current_date
```
Dieser Code verwendet die `now`-Methode der `Time`-Klasse, um das aktuelle Datum und die Uhrzeit abzurufen und es dann auf der Konsole auszugeben. Das Ergebnis sieht ungefähr so aus:

> 2021-09-30 16:30:00 +0200

Es ist auch möglich, das Datum und die Uhrzeit in einem bestimmten Format auszugeben, indem man der `now`-Methode ein Format-Argument übergibt. Zum Beispiel:

```Ruby
current_date = Time.now
puts current_date.strftime("%d.%m.%Y um %H:%M Uhr")
```

Dieser Code gibt das aktuelle Datum und die Uhrzeit im Format "Tag.Monat.Jahr um Stunde:Minute Uhr" aus, was zu folgendem Ergebnis führt:

> 30.09.2021 um 16:30 Uhr

## Deep Dive
Um das Datum und die Uhrzeit in noch mehr Details abzurufen, ist es hilfreich, die `Time`-Klasse genauer zu untersuchen. Zum Beispiel können wir mithilfe der `wday`-Methode den Wochentag des aktuellen Datums abrufen. Hier ist ein Codebeispiel:

```Ruby
current_date = Time.now
weekday = current_date.wday
puts "Heute ist ein #{weekday}."
```

Dieser Code gibt den Wochentag des aktuellen Datums als Zahl aus, wobei 0 für Sonntag, 1 für Montag, 2 für Dienstag, usw. steht. Um das Ergebnis lesbarer zu machen, wird die Zahl mit einer String-Interpolation verwendet und in den Namen des entsprechenden Wochentags übersetzt.

Außerdem bietet die `Time`-Klasse noch viele weitere nützliche Methoden, um das Datum und die Uhrzeit in verschiedenen Formaten abzurufen oder zu manipulieren. Um mehr darüber zu erfahren, empfehle ich die offizielle Ruby-Dokumentation zu konsultieren.

## Siehe auch
- [Offizielle Ruby-Dokumentation zur `Time`-Klasse](https://ruby-doc.org/core-3.0.2/Time.html)
- [Weitere Informationen über das Manipulieren von Datum und Uhrzeit in Ruby](https://www.rubyguides.com/2019/02/ruby-datetimes/)
- [Eine Liste von verschiedenen Formatierungs-Optionen für die `strftime`-Methode](https://apidock.com/ruby/DateTime/strftime)