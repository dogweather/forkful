---
title:    "PHP: Den aktuellen Datum erhalten"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/php/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Warum

Das Abrufen des aktuellen Datums ist ein wesentlicher Bestandteil der Programmierung in PHP. Es ermöglicht es uns, dynamische und zeitbezogene Inhalte in unsere Webanwendungen einzubinden. Außerdem kann es auch für die Verarbeitung von Daten und die Erstellung von Statistiken nützlich sein.

## Wie man das aktuelle Datum in PHP erhält

Das Abrufen des aktuellen Datums in PHP ist sehr einfach und kann auf verschiedene Arten durchgeführt werden. Wir können die Funktion `date()` verwenden, um das aktuelle Datum in verschiedenen Formaten zu erhalten. Hier ist ein Beispiel, wie man das aktuelle Datum im Format "Tag-Monat-Jahr" erhält:

```PHP
<?php
$current_date = date("d-m-Y");
echo "Das aktuelle Datum ist: " . $current_date;
// Das aktuelle Datum ist: 03-06-2020 
?>
```

Wir können auch die Funktion `date_default_timezone_set()` verwenden, um das Standard-Zeitzonenformat für unser Skript festzulegen. Dies ist nützlich, wenn wir mit verschiedenen Zeitzonen arbeiten müssen. Hier ist ein Beispiel, wie man die Zeitzone auf "Europa/Berlin" festlegt:

```PHP
<?php
date_default_timezone_set("Europe/Berlin");
$current_date = date("d-m-Y H:i:s");
echo "Das aktuelle Datum und die Uhrzeit sind: " . $current_date;
// Das aktuelle Datum und die Uhrzeit sind: 03-06-2020 15:30:00
?>
```

## Deep Dive

PHP bietet auch eine Reihe anderer Funktionen und Methoden, um das aktuelle Datum abzurufen und zu verarbeiten. Einige davon sind `getdate()`, `strtotime()` und `DateTime()`. Diese Funktionen ermöglichen es uns, das Datum in verschiedenen Formaten zu manipulieren, wie zum Beispiel das Hinzufügen oder Subtrahieren von Tagen, Monaten oder Jahren.

Es ist auch möglich, das aktuelle Datum mit einer bestimmten Sprache und Schreibweise auszugeben, indem man die Funktion `setlocale()` verwendet. Dies ist besonders nützlich, wenn man mehrsprachige Anwendungen entwickelt.

Eine detaillierte Dokumentation aller verfügbaren Funktionen und Methoden zur Datumsverarbeitung in PHP findest du [hier](https://www.php.net/manual/en/book.datetime.php).

## Siehe auch

- [How to Get the Current Date and Time in PHP](https://www.w3schools.com/php/php_date.asp)
- [Working with Dates and Time in PHP](https://www.tutorialrepublic.com/php-tutorial/php-date-and-time.php)
- [Manipulating Dates and Times in PHP](https://www.geeksforgeeks.org/manipulating-dates-and-times-in-php/)
- [PHP Date and Time Functions Reference](https://www.php.net/manual/en/ref.datetime.php)