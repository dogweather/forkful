---
title:                "Lesen von Kommandozeilenargumenten"
date:                  2024-01-20T17:56:29.101576-07:00
model:                 gpt-4-1106-preview
simple_title:         "Lesen von Kommandozeilenargumenten"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/php/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Lesen von Kommandozeilenargumenten erlaubt deinen PHP-Skripten, Eingaben direkt aus der Befehlszeile zu erhalten. Wir nutzen es, um Skripte flexibler zu gestalten und ermöglichen dem Nutzer, Variablen bei der Ausführung zu übergeben.

## So geht's:
Mit `$argv` und `$argc` kannst du auf die Argumente zugreifen:

```php
<?php
// test.php
if ($argc > 1) {
    echo "Hallo " . $argv[1] . "!\n";
} else {
    echo "Bitte gib deinen Namen an.\n";
}
```

Starte das Skript so:

```bash
$ php test.php Max
Hallo Max!
```

## Deep Dive
In PHP sind `$argv` und `$argc` globale Variablen, die Argumente aus der Kommandozeile erfassen – `argv` steht für "argument values" und `argc` für "argument count". 

Historisch gesehen ist diese Praxis aus den C-Programmiersprachen entstanden und wurde auch in PHP implementiert, um Skripten Kommandozeilenflexibilität zu geben. Alternativ gäbe es Funktionen wie `getopt()`, die eine detaillierte Analyse von Optionen ermöglichen. Aber für einfache Fälle reicht die Nutzung von `$argv` und `$argc` völlig aus.

Was wichtig ist: `php.ini` muss so konfiguriert sein, dass `register_argc_argv` auf **On** steht (standardmäßig der Fall). Ohne diese Einstellung stehen `$argv` und `$argc` nicht zur Verfügung.

## Siehe Auch
- Die offizielle PHP-Dokumentation zum Umgang mit Kommandozeilenparametern: [PHP: Command line usage](https://www.php.net/manual/en/features.commandline.php)
- Eine detaillierte Erklärung zu `getopt()`: [PHP: getopt](https://www.php.net/manual/en/function.getopt.php)
