---
title:    "PHP: Vergleich zweier Daten"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/php/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Warum
In der Programmierung muss oft das Datum verglichen werden, sei es um festzustellen, ob ein Termin bereits vorbei ist oder um zu überprüfen, ob ein bestimmtes Datum in der Zukunft liegt. In diesem Blogbeitrag werden wir uns ansehen, wie man auf einfache Weise zwei Daten in PHP vergleichen kann.

## Wie
Die einfachste Methode, um zwei Daten in PHP zu vergleichen, ist die Verwendung des Vergleichsoperators "==" oder "!=". Dieser Vergleichsoperator vergleicht den Wert der beiden Daten und gibt "true" zurück, wenn sie gleich sind, andernfalls "false". Hier ist ein Beispiel:

```PHP
<?php
$date1 = "2021-05-10";
$date2 = "2021-05-15";

if($date1 == $date2){
  echo "Die Daten sind gleich.";
} else{
  echo "Die Daten sind nicht gleich.";
}
```

Dieser Code würde "Die Daten sind nicht gleich." ausgeben, da $date1 und $date2 verschiedene Werte haben.

Um zu überprüfen, ob ein Datum vor oder nach einem anderen liegt, können wir auch den Vergleichsoperator "<" oder ">" verwenden. Hier ist ein Beispiel:

```PHP
<?php
$date1 = "2021-05-10";
$date2 = "2021-05-15";

if($date1 < $date2){
  echo "Das Datum 1 liegt vor Datum 2.";
} else{
  echo "Das Datum 2 liegt vor Datum 1.";
}
```

Dieser Code würde "Das Datum 1 liegt vor Datum 2." ausgeben, da $date1 fünf Tage vor $date2 liegt.

## Deep Dive
Wenn wir Daten genauer vergleichen und auch die Uhrzeit berücksichtigen möchten, können wir die Funktion "strtotime()" in PHP verwenden. Diese Funktion wandelt ein Datum in einen Unix-Timestamp um, der dann leichter verglichen werden kann. Hier ist ein Beispiel:

```PHP
<?php
$date1 = "2021-05-10 15:00:00";
$date2 = "2021-05-10 18:00:00";

// $timestamp1 enthält den Unix-Timestamp von $date1
$timestamp1 = strtotime($date1);

// $timestamp2 enthält den Unix-Timestamp von $date2
$timestamp2 = strtotime($date2);

// Wir können nun die Timestamps vergleichen
if($timestamp1 == $timestamp2){
  echo "Die Daten und Uhrzeiten sind gleich.";
}
```

Mit der Funktion "strtotime()" können wir auch verschiedene Datumsformate akzeptieren, wie zum Beispiel "May 10 2021 3:00 PM" oder "2021/05/10 3pm".

## Siehe auch
- [PHP Dateien vergleichen](https://www.php.net/manual/de/function.file-compare.php)
- [Datum und Uhrzeit in PHP formatieren](https://www.php.net/manual/de/function.date.php)
- [Unix-Timestamp in PHP](https://www.php.net/manual/de/function.strtotime.php)