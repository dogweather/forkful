---
title:    "PHP: Das Lesen von Befehlszeilenargumenten"
keywords: ["PHP"]
---

{{< edit_this_page >}}

## Warum

Die Verwendung von Befehlszeilenargumenten kann sehr hilfreich sein, um die Interaktion mit Benutzern zu verbessern oder komplexe Skripte auszuführen. Das Lesen von Befehlszeilenargumenten ermöglicht es Ihnen, dynamisch zu agieren und auf spezifische Benutzereingaben zu reagieren.

## Wie man Befehlszeilenargumente liest

Um Befehlszeilenargumente in PHP zu lesen, müssen Sie die globale Variable `$argv` verwenden. Diese Variable enthält ein Array mit allen eingegebenen Befehlszeilenargumenten. Hier ist ein Beispielcode:

```PHP
<?php
$argumente = $argv;

echo "Befehlszeilenargumente:" . PHP_EOL;
foreach ($argumente as $argument) {
    echo $argument . PHP_EOL;
}
```

Wenn Sie diesen Code ausführen und als Befehlszeilenargumente "hallo" und "welt" eingeben, wird die folgende Ausgabe erzeugt:

```
Befehlszeilenargumente:
hallo
welt
```

Sie können auch auf ein bestimmtes Argument in dem Array zugreifen, indem Sie den entsprechenden Index verwenden. Zum Beispiel, um auf das zweite Argument "welt" in unserem Beispiel zuzugreifen, können Sie `$argumente[1]` verwenden.

## Tiefere Einblicke

Wenn Sie mehr Kontrolle über die Behandlung der Befehlszeilenargumente benötigen, gibt es ein paar zusätzliche Funktionen, die nützlich sein könnten.

Die Funktion `count()` gibt die Anzahl der in einem Array enthaltenen Elemente zurück. Dies kann hilfreich sein, um zu überprüfen, ob ein bestimmtes Argument angegeben wurde oder nicht.

Die Funktion `array_shift()` entfernt das erste Element in einem Array und gibt es zurück. Dadurch können Sie das erste Argument in einer Variablen speichern und das Array für weitere Operationen kürzen.

Es gibt auch Möglichkeiten, mit benannten Argumenten oder Flaggen umzugehen, aber das würde den Rahmen dieses Artikels sprengen. Stattdessen empfehle ich, sich die offizielle PHP-Dokumentation für die `$argv`-Variable anzuschauen, um mehr zu erfahren.

## Siehe auch

- [PHP CLI-Handbuch](https://www.php.net/manual/de/features.commandline.php)
- [Offizielle PHP-Dokumentation zu $argv](https://www.php.net/manual/de/reserved.variables.argv.php)
- [Einführung in die Befehlszeile für PHP-Entwickler (auf Englisch)](https://kizu514.com/blog/cli-for-php-devs/)