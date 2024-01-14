---
title:    "PHP: Das Schreiben einer Textdatei"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/php/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Warum

Das Schreiben von Textdateien ist eine grundlegende Fähigkeit, die jeder PHP-Programmierer beherrschen sollte. Ob Sie Daten von einer externen Quelle speichern oder einfach nur eine einfache Ausgabedatei erstellen möchten, das Schreiben von Textdateien ist eine wichtige Fähigkeit, die sowohl in kleinen als auch in großen Projekten nützlich sein kann.

## Wie geht das?

Das Schreiben von Textdateien in PHP ist relativ einfach. Alles, was Sie tun müssen, ist eine neue Datei zu erstellen und sie mit Text zu füllen. Hier ist ein Beispiel dafür, wie man eine Datei mit dem Namen "test.txt" erstellt und einen einfachen Text hineinschreibt:

```PHP
<?php
    // Öffnen oder erstellen Sie eine Datei zum Schreiben
    $datei = fopen("test.txt", "w") or die("Kann Datei nicht öffnen!");

    // Schreiben Sie den Text in die Datei
    $text = "Dies ist ein Beispieltext.";
    fwrite($datei, $text);

    // Schließen Sie die Datei
    fclose($datei)
?>
```

Wenn Sie nun die Datei öffnen, werden Sie den geschriebenen Text darin sehen.

## Tiefentauchen

Natürlich gibt es noch viele weitere Funktionen und Optionen zum Schreiben von Textdateien in PHP. Sie können beispielsweise auch mehrere Zeilen in eine Datei schreiben oder die Datei in verschiedenen Modi öffnen (z.B. zum Anhängen von Text an eine bestehende Datei). Hier ist ein Beispiel für das Schreiben von mehreren Zeilen in eine Datei:

```PHP
<?php
    // Öffnen oder erstellen Sie eine Datei zum Schreiben
    $datei = fopen("test.txt", "w") or die("Kann Datei nicht öffnen!");

    // Definieren Sie den Text als Array von Zeilen
    $text = array(
        "Dies ist die erste Zeile.",
        "Dies ist die zweite Zeile.",
        "Und dies ist die dritte Zeile."
    );

    // Schreiben Sie jede Zeile in die Datei
    foreach($text as $zeile){
        fwrite($datei, $zeile . "\n");
    }

    // Schließen Sie die Datei
    fclose($datei);
?>
```

Wenn Sie die Datei öffnen, werden Sie nun alle drei Zeilen sehen.

## Siehe auch

- Die PHP [fopen](https://www.php.net/manual/de/function.fopen.php) Funktion
- Die PHP [fwrite](https://www.php.net/manual/de/function.fwrite.php) Funktion
- Mehr zum Thema "Textdateien schreiben" finden Sie in der offiziellen PHP-Dokumentation unter [Writing to files](https://www.php.net/manual/en/function.fwrite.php)