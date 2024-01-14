---
title:                "PHP: Das aktuelle Datum abrufen."
simple_title:         "Das aktuelle Datum abrufen."
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/php/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Warum

Das Abrufen des aktuellen Datums ist eine grundlegende und weit verbreitete Funktion in der PHP-Programmierung. Es ist wichtig, das aktuelle Datum in einer Anwendung zu verwenden, um beispielsweise Zeitstempel für Datenbankeintragungen oder die dynamische Anzeige von Zeit- oder Termininformationen zu generieren.

## Wie geht das?

Um das aktuelle Datum in PHP abzurufen, verwenden Sie die eingebaute Funktion `date()`. Diese Funktion akzeptiert einen Parameter, das sogenannte "Formatierungstoken", um das zurückgegebene Datum in unterschiedlichen Formaten anzuzeigen.

```PHP
$today = date("d.m.Y");
echo $today;
// Ausgabe: 06.10.2021
```

In dem obigen Beispiel wird das aktuelle Datum im Format "Tag.Monat.Jahr" zurückgegeben. Es gibt jedoch viele verschiedene Formatierungsmöglichkeiten für die `date()`-Funktion, die je nach Anwendungsfall ausgewählt werden können. Hier sind einige gängige Optionen:

```PHP
echo date("d.m.Y"); // 06.10.2021
echo date("d/m/Y"); // 06/10/2021
echo date("Y")." - ".date("d.m"); // 2021 - 06.10
echo date("l, d M. Y"); // Mittwoch, 06 Okt. 2021
```

Ein weiterer nützlicher Parameter für die `date()`-Funktion ist `time()`, mit dem das aktuelle Datum und die Uhrzeit zurückgegeben werden. Beispielsweise können wir das aktuelle Datum und die Uhrzeit mit folgendem Code anzeigen:

```PHP
echo "Heute ist ".date("d.m.Y")." und es ist ".date("H:i")." Uhr";
// Ausgabe: Heute ist 06.10.2021 und es ist 14:30 Uhr
```

## Tiefergehende Informationen

Die `date()`-Funktion ist Teil der PHP-Datums- und Zeitfunktionen. Diese enthalten auch viele andere nützliche Funktionen zur Manipulation und Formatierung von Datums- und Zeitwerten. Weitere Informationen und Beispiele zu diesen Funktionen finden Sie in der offiziellen PHP-Dokumentation [hier](https://www.php.net/manual/de/ref.datetime.php).

Sie können auch die Datumseinstellungen in Ihrer PHP-Konfiguration anpassen. Standardmäßig verwendet PHP die Systemkonfiguration, um das aktuelle Datum abzurufen, aber Sie können dies in der `php.ini`-Datei oder programmatisch in Ihrem Code ändern.

## Siehe auch

Weitere Informationen zur Datums- und Zeitmanipulation in PHP finden Sie auf den folgenden Websites:

- [PHP Datum- und Zeitfunktionen](https://www.php.net/manual/de/ref.datetime.php)
- [Häufig gestellte Fragen zu Datumsstempeln in PHP](https://www.php.net/manual/de/datetime.faq.php)
- [Einführung in die Verwendung von Datums- und Zeitstempeln in PHP](https://www.w3schools.com/php/php_date.asp)

Danke, dass Sie meinen Blogbeitrag gelesen haben. Ich hoffe, er war hilfreich für Sie!