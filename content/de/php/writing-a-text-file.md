---
title:                "Eine Textdatei schreiben"
html_title:           "PHP: Eine Textdatei schreiben"
simple_title:         "Eine Textdatei schreiben"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/php/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Was & Warum?
Textdateien schreiben ist ein grundlegender Teil der Programmierung, der es ermöglicht, Daten in einer lesbaren und speicherbaren Form zu speichern. Programmierer nutzen Textdateien, um Informationen wie Konfigurationen, Benutzereingaben oder Log-Daten zu speichern.

## Wie geht's:
```PHP
<?php
// Öffnen und beschreiben einer Textdatei
$fh = fopen("textdatei.txt", "w");
fwrite($fh, "Dies ist ein Beispieltext.");
fclose($fh);

// Lesen einer Textdatei und Ausgabe des Inhalts
$fh = fopen("textdatei.txt", "r");
$content = fread($fh, filesize("textdatei.txt"));
fclose($fh);
echo $content;
```

## Tiefentauchen:
Das Schreiben von Textdateien hat eine lange Geschichte und hat sich zusammen mit der Entwicklung der Programmierung weiterentwickelt. Alternativen zu Textdateien sind Binärdateien, die jedoch weniger lesbar und schwerer zu bearbeiten sind. Textdateien können auch in verschiedenen Formaten wie HTML oder CSV geschrieben werden. Beim Schreiben einer Textdatei ist es wichtig, die Zugriffsrechte und die Dateipfade sorgfältig zu berücksichtigen.

## Siehe auch:
- [PHP-Handbuch über das Schreiben von Dateien](https://www.php.net/manual/de/function.fwrite.php)
- [Wikipedia-Artikel über Textdateien](https://de.wikipedia.org/wiki/Textdatei)
- [Beitrag über Textdateien auf Stack Overflow](https://stackoverflow.com/questions/312446/how-do-i-create-a-file-and-write-to-it-in-php)