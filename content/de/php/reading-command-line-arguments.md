---
title:                "PHP: Einlesen von Befehlszeilenargumenten."
simple_title:         "Einlesen von Befehlszeilenargumenten."
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/php/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# Warum

Wenn Sie in der Welt der Programmierung sind, haben Sie wahrscheinlich schon von der Eingabeaufforderung gehört. Diese Befehlszeile ermöglicht es Ihnen, direkt mit Ihrem Computer zu kommunizieren und verschiedene Aktionen durchzuführen. Aber wussten Sie, dass Sie auch in PHP Befehlszeilenargumente lesen können? In diesem Blogbeitrag werden wir uns damit beschäftigen, warum es wichtig ist, Befehlszeilenargumente zu lesen und wie Sie es in Ihren eigenen Projekten anwenden können.

# Wie es geht

Das Lesen von Befehlszeilenargumenten ist in PHP unglaublich einfach. Es erfordert nur wenige Zeilen Code und kann in vielen verschiedenen Anwendungen nützlich sein. Schauen wir uns ein Beispiel an.

```PHP
<?php
// Prüfen ob ein Argument gegeben wurde
if (isset($argv[1])) {
  echo "Das gegebene Argument ist: " . $argv[1];
} else {
  echo "Es wurde kein Argument gegeben!";
}
?>
```

**Output:**

Wenn Sie das Script im Terminal ausführen und ein Argument geben, sollte die Ausgabe folgendermaßen aussehen:

```
$ php read_arguments.php Hallo Welt
Das gegebene Argument ist: Hallo Welt
```

In unserem Codebeispiel haben wir die Variable `$argv` verwendet, um auf das Argument zuzugreifen. Diese Variable enthält ein Array mit allen übergebenen Argumenten. Das erste Argument wird immer `$argv[1]` sein, da `$argv[0]` für den Pfad zur Skriptdatei reserviert ist.

# Deep Dive

Nun, da Sie wissen, wie einfach es ist, Befehlszeilenargumente in PHP zu lesen, wollen wir uns etwas tiefer damit beschäftigen. Wenn Sie mehrere Argumente an Ihr Skript übergeben möchten, können Sie einen `for`-Loop verwenden oder die Funktion `count()` angewenden, um die Anzahl der Argumente zu ermitteln. Außerdem können Sie die Argumente optional machen, indem Sie eine Standardwert für das Argument definieren, falls keines gegeben wurde.

Wenn Sie mehr über das Lesen von Befehlszeilenargumenten in PHP erfahren möchten, empfehle ich Ihnen, die offizielle Dokumentation zu lesen [hier](https://www.php.net/manual/en/reserved.variables.argv.php).

# Siehe auch

- [PHP CLI-Dokumentation](https://www.php.net/manual/en/features.commandline.php)
- [Command Line Arguments in PHP](https://www.tutorialspoint.com/php/php_command_line.htm)
- [PHP Command Line Scripts: Taming the Interface](https://www.sitepoint.com/php-command-line-scripts-taming-the-interface/)