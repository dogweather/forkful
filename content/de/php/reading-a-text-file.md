---
title:                "Das Lesen einer Textdatei"
html_title:           "PHP: Das Lesen einer Textdatei"
simple_title:         "Das Lesen einer Textdatei"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/php/reading-a-text-file.md"
---

{{< edit_this_page >}}

# Was & Warum?
Das Lesen von Textdateien ist eine häufige Aufgabe für PHP-Programmierer. Es ermöglicht ihnen, Daten aus einer Datei zu lesen und in ihrer Anwendung zu verwenden.

# Wie geht's?
Du kannst eine Textdatei in PHP ganz einfach lesen, indem du die Datei mit der Funktion "fopen()" öffnest und dann Zeile für Zeile mit der Funktion "fgets()" liest. Hier ist ein Beispiel:
```PHP
<?php
$handle = fopen("textdatei.txt", "r"); // Öffne die Datei im "Lesen"-Modus
if ($handle) { // Überprüfe, ob die Datei erfolgreich geöffnet wurde
  while (($line = fgets($handle)) !== false) { // Lese eine Zeile aus der Datei
    echo $line; // Gib die Zeile aus
  }
  fclose($handle); // Schließe die Datei
} else {
  echo "Datei konnte nicht geöffnet werden."; // Gib eine Fehlermeldung aus, falls die Datei nicht geöffnet werden konnte
}
?>
```
Dieses Beispiel öffnet eine Datei namens "textdatei.txt" und liest jede Zeile daraus, die dann mit "echo" ausgegeben wird. Am Ende wird die Datei wieder geschlossen.

# Tiefer Einblick
Das Lesen von Textdateien in PHP ist nichts Neues - es wird seit den Anfängen der Sprache unterstützt. Es ist jedoch die einfachste Methode, um Daten aus einer Datei zu lesen und wird daher häufig verwendet.

Eine Alternative zum Lesen von Textdateien in PHP ist die Verwendung von Datenbanken, um Daten zu speichern und abzurufen. Dies ist jedoch mit zusätzlichem Aufwand verbunden, während das Lesen von Textdateien sehr direkt und einfach ist.

Technisch gesehen wird beim Lesen einer Textdatei in PHP jeder Zeilenwechsel als ein Zeichen behandelt. Dies bedeutet, dass die resultierenden Strings in Ihrer Anwendung möglicherweise zusätzliche Zeilenumbrüche enthalten, die Sie möglicherweise entfernen müssen.

# Siehe auch
Weitere Informationen zu "fopen()" und "fgets()": https://www.php.net/manual/de/function.fopen.php und https://www.php.net/manual/de/function.fgets.php