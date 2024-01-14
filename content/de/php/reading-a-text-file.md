---
title:                "PHP: Eine Textdatei lesen"
simple_title:         "Eine Textdatei lesen"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/php/reading-a-text-file.md"
---

{{< edit_this_page >}}

# Warum

Das Einlesen von Textdateien ist ein grundlegender Schritt in der Programmierung. Es ermöglicht uns, Daten aus externen Quellen in unser Programm zu integrieren und weiterzuverarbeiten. In diesem Blog-Beitrag werden wir zeigen, wie man dies in PHP erreichen kann.

# Wie man Textdateien in PHP liest

Das Einlesen von Textdateien in PHP ist relativ einfach. Zunächst müssen wir die Datei öffnen und dann die Zeilen der Datei auslesen und in einer Variablen speichern. Dies kann mit der Funktion `file()` erreicht werden:

```PHP
$file = file("meine_datei.txt");
```

Diese Funktion liest die gesamte Datei ein und gibt die Zeilen als Array zurück. Wir können jetzt durch dieses Array iterieren und die Daten weiterverarbeiten:

```PHP
foreach($file as $line) {
    echo $line . "<br>";
}
```

In diesem Beispiel geben wir einfach jede Zeile der Datei aus. Natürlich kann man hier je nach Bedarf auch andere Aktionen durchführen, wie zum Beispiel das Speichern der Daten in einer Datenbank.

# Tiefergehende Informationen

Es gibt verschiedene Funktionen in PHP, die beim Einlesen von Textdateien hilfreich sein können. Eine davon ist `fgets()`, welche eine Zeile aus der Datei ausliest und sie als String zurückgibt. Wir können auch die Position in der Datei mit der Funktion `ftell()` auslesen und mit `fseek()` zu einer bestimmten Position in der Datei springen.

Außerdem ist es wichtig, beim Einlesen von Textdateien auf die Dateicodierung zu achten, um Sonderzeichen richtig zu verarbeiten. In PHP gibt es dafür die Funktion `mb_convert_encoding()`, die die Codierung der Datei ändert.

# Siehe auch

- Offizielle Dokumentation zu Dateifunktionen in PHP: https://www.php.net/manual/de/ref.filesystem.php
- Tutorial zum Einlesen von Textdateien in PHP: https://www.php-einfach.de/php-tutorial/ein-ausgabe-dateizugriffe/einlesen-textdateien/