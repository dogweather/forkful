---
title:    "PHP: Erstellen einer temporären Datei"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/php/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# Warum

Ein temporäres (oder auch zeitweiliges) File wird häufig von Programmierern erstellt, um komplexe Operationen durchzuführen, die ein Zwischenergebnis benötigen. Zum Beispiel kann ein temporäres File genutzt werden, um Daten einfacher zu speichern, nach bestimmten Bedingungen zu filtern oder um sie zu sortieren, bevor sie in die endgültige Datei geschrieben werden.

# Wie geht's

Um ein temporäres File in PHP zu erstellen, können wir die `tempnam()` Funktion nutzen. Diese Funktion erstellt eine temporäre Datei mit einem zufälligen Dateinamen und Pfad. Wir können diese Funktion wie folgt in unserem Code verwenden:

```PHP
$temp_file = tempnam(sys_get_temp_dir(), "prefix_");
echo "Temporäres File: " . $temp_file;

// Output: Temporäres File: /tmp/prefix_7XIUAt
```

In diesem Beispiel haben wir `tempnam()` mit zwei Parametern aufgerufen. Der erste Parameter ist der Pfad zum temporären Verzeichnis auf dem Server und der zweite ist der Präfix, der in den generierten Dateinamen eingefügt wird. Wir können den zweiten Parameter ignorieren und `tempnam()` wird automatisch einen Präfix erstellen.

# Tief eintauchen

Die `tempnam()` Funktion generiert einen eindeutigen Dateinamen und erstellt automatisch die Datei. Wenn wir jedoch nur einen Dateinamen benötigen, können wir die `tmpfile()` Funktion verwenden, die nur den Dateinamen zurückgibt und die Datei nicht erstellt.

Was passiert, wenn wir ein temporäres File erstellen, aber es nicht mehr benötigen? Es ist wichtig, dass wir das File nach der Verwendung löschen, um Speicherplatz freizugeben. Hier können wir die `unlink()` Funktion verwenden, um das temporäre File zu löschen:

```PHP
$temp_file = tempnam(sys_get_temp_dir(), "prefix_");
echo "Temporäres File: " . $temp_file;

// Output: Temporäres File: /tmp/prefix_KH51fQ

unlink($temp_file); // löscht das temporäre File
```

Abschließend ist es wichtig zu beachten, dass temporäre Files nur für kurzzeitige Verwendung gedacht sind und nicht für die langfristige Speicherung von Daten geeignet sind.

# Siehe auch

- [PHP Dokumentation: tempnam()](https://www.php.net/manual/de/function.tempnam.php)
- [PHP Dokumentation: tmpfile()](https://www.php.net/manual/de/function.tmpfile.php)
- [PHP Dokumentation: unlink()](https://www.php.net/manual/de/function.unlink.php)