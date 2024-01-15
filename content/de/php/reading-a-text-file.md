---
title:                "Lesen einer Textdatei"
html_title:           "PHP: Lesen einer Textdatei"
simple_title:         "Lesen einer Textdatei"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/php/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Warum

Wenn Sie sich mit der Programmiersprache PHP beschäftigen, werden Sie wahrscheinlich irgendwann auf die Aufgabe stoßen, eine Textdatei zu lesen. Das Lesen einer Textdatei kann hilfreich sein, wenn Sie Benutzereingaben verarbeiten oder Daten aus einer externen Quelle in Ihr Skript einbinden möchten.

## Wie es geht

Das Lesen einer Textdatei in PHP ist relativ einfach. Zunächst müssen Sie die Datei mit der Funktion `fopen()` öffnen, die als Parameter den Dateipfad und den gewünschten Modus (z.B. "r" für lesen) erhält.

````PHP
$file = fopen("meine_datei.txt", "r"); //Öffnet die Datei zum Lesen
````

Anschließend können Sie die Datei zeilenweise mit der Funktion `fgets()` lesen und den Inhalt in einer Variable speichern.

````PHP
$inhalt = fgets($file); //Liest die erste Zeile der Datei
````

Um alle Zeilen einer Datei zu lesen, können Sie eine Schleife verwenden.

````PHP
while (!feof($file)) { //Solange das Ende der Datei nicht erreicht ist
    $inhalt = fgets($file); //Liest die nächste Zeile der Datei
    echo $inhalt; //Gibt den Inhalt aus
}
````

Um die Datei nach dem Lesen wieder zu schließen, verwenden Sie die Funktion `fclose()`.

````PHP
fclose($file); //Schließt die Datei
````

## Tiefen-Tauchgang

PHP bietet auch noch weitere Funktionen für das Lesen von Textdateien, wie z.B. `file_get_contents()`, mit der Sie den gesamten Inhalt einer Datei in eine Variable laden können.

````PHP
$inhalt = file_get_contents("meine_datei.txt"); //Lädt den Inhalt der Datei in eine Variable
````

Auch das Öffnen von Dateien im Binärmodus (z.B. für Bilder oder PDFs) ist möglich mit der Funktion `fopen()` und dem entsprechenden Modus (z.B. "rb" für lesen im Binärmodus).

## Sieh dir auch an

- [PHP Handbuch zu Dateien](https://www.php.net/manual/de/ref.filesystem.php)
- [Tutorial: Dateien mit PHP lesen und schreiben](https://www.php-einfach.de/php-tutorial/dateien-ein-und-auslesen/)
- [PHP: Do's and Don'ts beim Dateizugriff](https://www.sitepoint.com/php-file-access/)