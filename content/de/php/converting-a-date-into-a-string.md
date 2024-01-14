---
title:                "PHP: Eine Datum in einen String umwandeln."
simple_title:         "Eine Datum in einen String umwandeln."
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/php/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Warum

Das Konvertieren eines Datums in einen String ist ein häufiger Vorgang in der Programmierung. Es ermöglicht uns, Daten, die in einer numerischen Form gespeichert sind, lesbarer zu machen und sie in verschiedenen Anwendungen zu verwenden.

## Wie geht man vor

Um ein Datum in einen String umzuwandeln, gibt es verschiedene Methoden, je nachdem welche Programmiersprache verwendet wird. Im Folgenden werde ich zeigen, wie man dies in PHP erreichen kann.

```PHP
$date = strtotime("2021-12-31"); //erstellt ein Datum (31. Dezember 2021)
$output = date("d.m.Y", $date); //wandelt das Datum in ein String-Format um (31.12.2021)
echo $output; //gibt den String "31.12.2021" aus
```

In diesem Beispiel wird die Funktion `strtotime()` verwendet, um das Datum in ein numerisches Format umzuwandeln. Anschließend wird die Funktion `date()` benutzt, um dieses Format in ein lesbares Datum umzuwandeln. Diese Funktion akzeptiert auch verschiedene Formatierungsmöglichkeiten, um das Datum nach eigenen Bedürfnissen anzupassen.

## Tiefer Einblick

In der Programmierung gibt es verschiedene Datums- und Zeitformate, wie z.B. Unix Timestamps oder ISO 8601. Indem man ein Datum in einen String konvertiert, können wir sicherstellen, dass es in einem einheitlichen Format vorliegt und leichter lesbar für Benutzer oder andere Programme ist.

Um ein besseres Verständnis für die Formatierungsmöglichkeiten von `date()` zu bekommen, kann man die offizielle Dokumentation von PHP konsultieren und verschiedene Beispiele ausprobieren. Es ist auch wichtig zu beachten, dass die Funktion `date()` das aktuelle Zeit- und Datumsformat des Systems verwendet. Wenn man jedoch international agiert und ein bestimmtes Format benötigt, kann man die Funktion `setlocale()` verwenden, um die Standardzeitzone und Sprache festzulegen.

## Siehe auch

- [PHP-Handbuch zu Dateiformaten](https://www.php.net/manual/de/datetime.format.php)
- [ISO 8601 Format Guide](https://www.iso.org/iso-8601-date-and-time-format.html)
- [PHP-Handbuch zu Datums- und Zeitfunktionen](https://www.php.net/manual/de/ref.datetime.php)