---
title:    "PHP: Aktuelles Datum abrufen"
keywords: ["PHP"]
---

{{< edit_this_page >}}

## Warum
Die Verwendung des aktuellen Datums ist ein grundlegender Aspekt in der Programmierung, besonders in der Webentwicklung. Jedoch kann es manchmal verwirrend sein, wie man das aktuelle Datum in PHP abruft. In diesem Blog-Beitrag werden wir uns ansehen, warum es wichtig ist, das aktuelle Datum in PHP zu erhalten und wie man es auf verschiedene Weisen tun kann.

## Wie man das aktuelle Datum in PHP abruft
Um das aktuelle Datum in PHP abzurufen, gibt es mehrere Methoden. Ein einfacher Weg ist die Verwendung der vordefinierten Funktion `date()`. Schauen wir uns folgendes Beispiel an:

```PHP
$current_date = date("d.m.y"); 
echo $current_date;
```
Das würde die Ausgabe `02.08.19` erzeugen, je nachdem, welches Datum zu dem Zeitpunkt, an dem der Code ausgeführt wird, aktuell ist. Man kann auch das aktuelle Datum in einem bestimmten Format erhalten, indem man den Parametern in der `date()` Funktion Werte zuweist. Zum Beispiel:

```PHP 
$current_date = date("l, F jS, Y"); 
echo $current_date;
```

Das würde die Ausgabe `Freitag, August 2ten, 2019` erzeugen. Es gibt viele verschiedene Parameter, die man verwenden kann, um das Datum in verschiedenen Formaten zu erhalten. Man kann diese Parameter in der offiziellen [PHP Dokumentation](https://www.php.net/manual/en/function.date.php) finden.

Man kann auch das aktuelle Datum mit dem `DateTime` Objekt abrufen. Dies bietet zusätzliche Funktionen, um das Datum zu manipulieren und Berechnungen durchzuführen. Schauen wir uns ein Beispiel an, das das aktuelle Datum um 7 Tage zukünftig ausgibt:

```PHP
$current_date = new DateTime(); 
$future_date = $current_date->add(new DateInterval("P7D"));
echo $future_date->format("d-m-y");
```
Das würde die Ausgabe `09-08-19` erzeugen.

## Tiefere Einblicke
Wenn man das Datum verwendet, ist es wichtig zu wissen, dass die Zeitzone, in der man sich befindet, eine Rolle spielt. Man kann dies im Code mithilfe der `date_default_timezone_set()` Funktion festlegen. Zum Beispiel:

```PHP 
date_default_timezone_set("Europe/Berlin");
```
Das stellt sicher, dass das Datum in der Zeitzone von Berlin angezeigt wird. Wenn man sich nicht sicher ist, welche Zeitzone man verwenden sollte, kann man die offizielle [Liste der unterstützten Zeitzonen](https://www.php.net/manual/en/datetimezone.listidentifiers.php) in der PHP Dokumentation überprüfen.

Ein weiterer wichtiger Aspekt ist die Genauigkeit des Datums. In PHP gibt es verschiedene Arten von Datums- und Zeitfunktionen, die verschiedene Genauigkeiten aufweisen. Es ist wichtig, die richtige Funktion für den Verwendungszweck zu wählen, um unerwünschte Ergebnisse zu vermeiden.

## Siehe auch
- [Offizielle PHP Dokumentation - Datum und Zeit Funktionen](https://www.php.net/manual/en/ref.datetime.php)
- [Tutorial: Das Datum in PHP verstehen](https://www.tutorialrepublic.com/php-tutorial/php-date-and-time.php)
- [PHP-Zeitzonenliste](https://www.php.net/manual/en/datetimezone.listidentifiers.php)