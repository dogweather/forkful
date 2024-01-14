---
title:    "Gleam: Ein Datum in einen String umwandeln"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/gleam/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Warum

Die Umwandlung von Datum zu String ist in der Programmierung ein häufiger Bedarf. Es ermöglicht, ein Datum visuell in einem Format darzustellen, das für den Benutzer leichter lesbar ist. In diesem Blogbeitrag werden wir sehen, wie man dies mit der Programmiersprache Gleam erreichen kann.

## Wie geht das?

Um ein Datum in String umzuwandeln, gibt es ein eingebautes Modul in Gleam namens `gleam/time`. Innerhalb dieses Moduls gibt es die Funktion `format_date`, die genau das tut, wonach wir suchen. Schauen wir uns ein Beispiel an:

```
Gleam
import gleam/time
import gleam/string
import gleam/stdio.{stdout, println}

let date_to_convert = time.date(2021, 10, 20)
let converted_date = time.format_date("%d.%m.%Y", date_to_convert)
stdout
|> println(converted_date)
```

Ausgabe: `20.10.2021`

Hier haben wir das Datum "20. Oktober 2021" als `date_to_convert` definiert und dann die Funktion `format_date` verwendet, um es in das gewünschte Format umzuwandeln. Das erste Argument, `"%d.%m.%Y"`, gibt an, wie das Datum dargestellt werden soll. In diesem Fall steht `%d` für den Tag, `%m` für den Monat und `%Y` für das Jahr. Die Reihenfolge und das Format kann je nach Belieben geändert werden. Wir haben dann die Variable `converted_date` definiert, die den konvertierten String enthält. Schließlich haben wir die Ausgabe mit der `stdout`-Funktion auf der Konsole ausgegeben.

Dies ist nur ein einfaches Beispiel, aber es zeigt, wie einfach es ist, ein Datum in einen String umzuwandeln. Gleam bietet auch Funktionen, um die Zeitzone und die Uhrzeit zu berücksichtigen, falls das benötigt wird.

## Tiefer eintauchen

In diesem Abschnitt können wir uns noch genauer damit beschäftigen, wie die `format_date`-Funktion funktioniert und welche anderen Optionen es gibt. Die vollständige Dokumentation für das `gleam/time`-Modul kann hier gefunden werden: [https://gleam.run/modules/gleam/time.html](https://gleam.run/modules/gleam/time.html)

Es ist auch wichtig zu beachten, dass Gleam auf der Programmiersprache Erlang aufbaut, die bereits eine eingebaute Funktion hat, um ein Datum in String zu konvertieren. Dies bedeutet, dass, falls Gleam in Zukunft nicht mehr verwendet wird, das Wissen über die Nutzung von `format_date` auch auf andere Programmiersprachen übertragen werden kann.

## Siehe auch

- [Dokumentation für das 'gleam/time'-Modul](https://gleam.run/modules/gleam/time.html)
- [Erlang 'calendar'-Modul](https://erlang.org/doc/man/calendar.html)