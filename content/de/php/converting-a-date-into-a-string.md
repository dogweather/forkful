---
title:    "PHP: Ein Datum in einen String umwandeln"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/php/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Warum

In der Programmierung ist es oft erforderlich, ein Datum in Form eines Strings darzustellen. Dies kann zum Beispiel für eine Benutzeroberfläche oder für das Speichern von Daten in einer Datenbank notwendig sein. In diesem Blog-Beitrag werden wir uns genauer ansehen, wie man ein Datum in PHP in einen String konvertieren kann und warum dies nützlich ist.

## Wie geht das?

Die Konvertierung eines Datums in einen String kann mit der Funktion `date()` in Kombination mit einem Formatierungsstring durchgeführt werden. Hier ist ein Beispiel:

```PHP
$date = date('d.m.Y', time());
echo $date;
```

In diesem Beispiel wird das aktuelle Datum in das gewünschte Format "Tag.Monat.Jahr" konvertiert und dann ausgegeben. Das Ergebnis würde beispielsweise "13.09.2021" lauten.

Ein weiteres Beispiel ist die Nutzung des `DateTime`-Objekts, um ein Datum in einen String zu konvertieren:

```PHP
$date = new DateTime();
echo $date->format('M d, Y');
```

In diesem Fall wird das aktuelle Datum in das Format "Monat Tag, Jahr" konvertiert und ausgegeben. Das Ergebnis wäre zum Beispiel "Sep 13, 2021".

## Tiefergehende Analyse

Die `date()`-Funktion bietet eine Vielzahl von Möglichkeiten, um Datumsangaben in Strings zu formatieren. Es gibt verschiedene Formate für Tage, Monate, Jahre, Wochentage, Uhrzeiten und vieles mehr. Die vollständige Liste der möglichen Formatierungen kann in der offiziellen PHP-Dokumentation gefunden werden.

Darüber hinaus ist es wichtig zu beachten, dass die Konvertierung eines Datums in einen String auch sprachabhängig sein kann. In PHP gibt es die Funktion `setlocale()`, mit der die Datumsformatierung an die Standardsprache des Systems angepasst werden kann.

In einigen Fällen kann es auch erforderlich sein, die Zeitzone des Systems zu berücksichtigen. Dies kann mit der Funktion `date_default_timezone_set()` festgelegt werden.

## Siehe auch

Weitere Informationen zur Konvertierung von Daten in Strings in PHP finden Sie unter den folgenden Links:

- [PHP: Date() Funktion](https://www.php.net/manual/de/function.date.php)
- [PHP: DateTime Objekt](https://www.php.net/manual/de/class.datetime.php)
- [PHP: setlocale() Funktion](https://www.php.net/manual/de/function.setlocale.php)
- [PHP: date_default_timezone_set() Funktion](https://www.php.net/manual/de/function.date-default-timezone-set.php)