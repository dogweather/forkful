---
title:                "Lesen von Befehlszeilenargumenten"
html_title:           "PHP: Lesen von Befehlszeilenargumenten"
simple_title:         "Lesen von Befehlszeilenargumenten"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/php/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Was & Warum?

Das Lesen von Befehlszeilenargumenten ist ein grundlegender Teil der PHP Programmierung. Dabei handelt es sich um die Möglichkeit, Informationen, die bei der Ausführung eines PHP Skripts angegeben werden, abzurufen. Programmierer nutzen diese Funktion, um z.B. Daten zu verarbeiten, die von einem Nutzer über die Kommandozeile eingegeben wurden.

## Wie geht's?

Um Befehlszeilenargumente in PHP auszulesen, gibt es die Funktion "getopt()". Diese Funktion erwartet zwei Parameter: die Optionen, die das Skript unterstützen soll, und die eingegebenen Argumente. Hier ein Beispiel:

```PHP
<?php
$erlaubteOptionen = "f:h";
$eingabeargumente = getopt($erlaubteOptionen);

if (array_key_exists("f", $eingabeargumente)) {
    $datei = $eingabeargumente["f"];
    echo "Datei: " . $datei . "\n";
} elseif (array_key_exists("h", $eingabeargumente)) {
    echo "Hilfe: Zeige alle verfügbaren Optionen\n";
} else {
    echo "Bitte geben Sie eine Datei an oder verwenden Sie die Option -h für Hilfe.\n";
}
?>
```

Angenommen, das Skript heißt "beispiel.php" und liegt im selben Verzeichnis wie eine Datei mit dem Namen "test.txt", dann kann man es auf folgende Weise ausführen:

`php beispiel.php -f test.txt`

Und die Ausgabe sieht dann so aus:

`Datei: test.txt`

## Tiefere Einblicke

Das Auslesen von Befehlszeilenargumenten ist in PHP schon seit der Einführung der Version 4 möglich. Alternative Funktionen wie "getopt_long()" bieten erweiterte Möglichkeiten, um Optionen und Argumente zu verarbeiten. Auch die Verwendung von Bibliotheken wie "Symfony Console" kann die Handhabung von Befehlszeilenargumenten erleichtern.

## Weitere Informationen

Hier noch ein paar Links für weitere Informationen und Beispiele zur Verwendung von Befehlszeilenargumenten in PHP:

- Die offizielle Dokumentation: https://www.php.net/manual/de/function.getopt.php
- PHP.net Tutorial: https://www.php.net/manual/de/features.commandline.php
- Beispielprojekt auf GitHub: https://github.com/php/cli-tools
- Symfony Console: https://symfony.com/doc/current/components/console.html