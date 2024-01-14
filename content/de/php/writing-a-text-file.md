---
title:                "PHP: Das Schreiben einer Textdatei"
programming_language: "PHP"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/php/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Warum

Das Schreiben von Textdateien ist ein wichtiger Teil der Programmierung, besonders in PHP. Durch das Schreiben von Textdateien können Daten gespeichert und später wieder abgerufen werden. Dies ist besonders nützlich, wenn es darum geht, große Datenmengen zu verarbeiten oder permanent zu speichern.

## How To

Um eine Textdatei in PHP zu schreiben, benötigt man zunächst einen Datei-Handle, der die Datei öffnet und schreibbereit macht. Dies kann mit der Funktion `fopen()` erreicht werden:

```PHP
$handle = fopen("filename.txt", "w") or die("Unable to open file!");
```

In diesem Beispiel wird die Datei "filename.txt" zur Bearbeitung geöffnet und der Handle in der Variablen `$handle` gespeichert. Der zweite Parameter `"w"` steht für den Schreibmodus (write), was bedeutet, dass wir in die Datei schreiben können.

Nun können wir den `$handle` verwenden, um Zeilen in die Textdatei zu schreiben, zum Beispiel:

```PHP
$txt = "Dies ist ein Beispieltext, der in unsere Datei geschrieben wird.";
fwrite($handle, $txt);
```

Dieser Code schreibt den Inhalt der Variablen `$txt` in die geöffnete Datei. Es ist auch möglich, mehrere Zeilen nacheinander in die Datei zu schreiben, indem man die `fwrite()` Funktion entsprechend oft aufruft.

Abschließend muss die Datei wieder geschlossen werden, um sie zu speichern und freizugeben. Dies kann mit der Funktion `fclose()` erreicht werden:

```PHP
fclose($handle);
```

Wenn alles erfolgreich war, wird nun eine neue Textdatei mit dem Namen "filename.txt" erstellt und der gewünschte Inhalt wird in die Datei geschrieben.

## Deep Dive

Bei der Verwendung der `fwrite()` Funktion ist es wichtig, die richtigen Parameter zu verwenden. Wenn man anstelle von `fwrite($handle, $txt)` die Parameter `fwrite($txt, $handle)` verwendet, wird PHP versuchen, den Inhalt der Datei `$txt` in den Handle `$handle` zu schreiben, was zu einem Fehler führen wird. 

Außerdem ist es wichtig, zu überprüfen, ob die Datei erfolgreich geöffnet werden konnte, bevor man versucht, in sie zu schreiben, um Fehler zu vermeiden. Dies kann durch Hinzufügen des `or die("Unable to open file!")` Teils beim Öffnen des Handles erreicht werden.

## Siehe auch

- [PHP fopen() Funktion](https://www.php.net/manual/de/function.fopen.php)
- [PHP fwrite() Funktion](https://www.php.net/manual/de/function.fwrite.php)
- [PHP fclose() Funktion](https://www.php.net/manual/de/function.fclose.php)