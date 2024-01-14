---
title:    "Ruby: Ein Datum in einen String umwandeln"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

## Warum

Warum sollte man sich mit der Umwandlung eines Datums in einen String beschäftigen? Die Antwort lautet: Es ist eine grundlegende Fähigkeit, die in vielen Programmen benötigt wird. Es ermöglicht uns, Daten in einem benutzerfreundlichen Format anzuzeigen und zu verarbeiten.

## Anleitung

Um ein Datum in einen String umzuwandeln, können wir die `strftime`-Funktion verwenden. Diese Funktion nimmt zwei Argumente an: Das Format, in dem das Datum angezeigt werden soll, und das Datum selbst. Hier ist ein Beispiel:

```Ruby
date = Time.new(2021, 8, 13)
date.strftime("%d.%m.%Y")
```

Dieser Code würde den 13. August 2021 im deutschen Format (Tag.Monat.Jahr) ausgeben: "13.08.2021". Wir können auch andere Formate verwenden, z.B. "%B %Y" würde nur den Monat und das Jahr anzeigen: "August 2021". Hier ist eine Liste mit den gängigsten Formatierungsoptionen:

- %Y: Vierstellige Jahreszahl
- %m: Monat als zweistellige Zahl
- %d: Tag als zweistellige Zahl
- %B: Vollständiger Monatsname
- %b: Abgekürzter Monatsname
- %A: Vollständiger Wochentag
- %a: Abgekürzter Wochentag

Für eine vollständige Liste und detailliertere Informationen empfehle ich die offizielle Ruby-Dokumentation zur `strftime`-Funktion.

## Tiefgehend

Beim Umwandeln eines Datums in einen String gibt es einige Dinge zu beachten. Zum Beispiel können wir mit der `strftime`-Funktion nicht nur das aktuelle Datum, sondern auch zukünftige oder vergangene Daten formatieren. Außerdem können wir die Zeitzone des Datums berücksichtigen und angeben, ob wir das Datum in verschiedenen Sprachen darstellen möchten, indem wir die Locale-Einstellungen ändern.

Es ist auch wichtig zu bedenken, dass die Funktion `strftime` nur für Ruby-Datumsobjekte funktioniert. Wenn wir ein Datum aus einem anderen Format haben (z.B. als String), müssen wir es zunächst in ein Ruby-Datumsobjekt umwandeln, bevor wir es formatieren können.

## Siehe auch

- Offizielle Ruby-Dokumentation zur `strftime`-Funktion: https://ruby-doc.org/stdlib-2.7.2/libdoc/date/rdoc/Date.html#method-i-strftime
- Blog-Post über Datumsmanipulation in Ruby (auf Deutsch): https://www.bigbinary.com/blog/date-and-time-manipulation-in-ruby