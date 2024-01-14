---
title:                "Gleam: Das aktuelle Datum abrufen"
programming_language: "Gleam"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/gleam/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Warum

In diesem Blogpost werden wir uns damit beschäftigen, wie man das aktuelle Datum mit der Programmiersprache Gleam abrufen kann. Aber warum sollte man überhaupt das aktuelle Datum abrufen wollen?

Das aktuelle Datum kann in vielen Anwendungsfällen nützlich sein. Zum Beispiel wenn wir eine Anwendung haben, die abhängig vom Datum unterschiedliche Prozesse ausführen soll. Oder wenn wir einfach nur das Datum in unserem Programm anzeigen wollen. Mit Gleam ist es einfach, das aktuelle Datum zu bekommen und es für unsere Bedürfnisse zu nutzen.

# Wie geht das?

Um das aktuelle Datum in Gleam abzurufen, müssen wir die Funktion `DateTime.now()` verwenden. Diese Funktion gibt uns ein Objekt vom Typ `DateTime` zurück, welches das aktuelle Datum und die aktuelle Uhrzeit enthält.

Lassen Sie uns einen Blick auf ein einfaches Beispiel werfen:

```Gleam
let current_date = DateTime.now()
io.println("Das aktuelle Datum ist: #{current_date.date}")
```

Dieses Beispiel nutzt die Funktion `io.println()` um das aktuelle Datum auf der Konsole auszugeben. Dabei greifen wir auf das Feld `date` des `DateTime` Objekts zu, um nur das Datum ohne die Uhrzeit auszugeben.

Das oben genannte Beispiel gibt zum Beispiel die Ausgabe `Das aktuelle Datum ist: 2020-06-23` aus.

# Tiefer Einblick

In Gleam wird das Datum im ISO-8601-Format gespeichert, was bedeutet, dass es folgendes Format hat: `YYYY-MM-DD`. Wenn wir also das aktuelle Datum abrufen, erhalten wir es im Format `YYYY-MM-DD` zurück. Wir können auch auf die Uhrzeit und andere Informationen wie Wochentag oder Zeitzone zugreifen, indem wir auf die entsprechenden Felder des `DateTime` Objekts zugreifen.

Es gibt auch weitere Funktionen, die es uns erlauben, das aktuelle Datum auf verschiedene Weisen zu manipulieren, zum Beispiel `DateTime.add()` um Tage oder Monate hinzuzufügen oder `DateTime.diff()` um die Differenz zwischen zwei Daten zu berechnen.

In der offiziellen Dokumentation von Gleam können Sie weitere Informationen über die `DateTime` Struktur und ihre Funktionen finden.

# Siehe auch

- [Offizielle Gleam Dokumentation](https://gleam.run/documentation/)
- [ISO-8601-Format](https://en.wikipedia.org/wiki/ISO_8601)