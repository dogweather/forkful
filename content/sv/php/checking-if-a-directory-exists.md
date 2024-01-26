---
title:                "Kontrollera om en katalog finns"
date:                  2024-01-20T14:57:42.494243-07:00
html_title:           "Fish Shell: Kontrollera om en katalog finns"
simple_title:         "Kontrollera om en katalog finns"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/php/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why?
Att kolla om en mapp finns är att säkerställa att en specifik katalog verkligen existerar i filsystemet innan man försöker använda den. Programmerare gör detta för att undvika fel och undantag som kan kastas om de antar att en katalog finns när den i själva verket inte gör det.

## How to:
Använd `is_dir()` för att kolla om en mapp finns:

```PHP
$directory = "/path/to/your/directory";

if (is_dir($directory)) {
    echo "Mappen finns!";
} else {
    echo "Mappen finns inte.";
}
```
Utskrift kan vara:
```
Mappen finns!
```
eller
```
Mappen finns inte.
```

## Deep Dive
Funktionen `is_dir()` har funnits länge i PHP och är fortfarande det primära sättet att kolla om en mapp finns. Alternativ inkluderar `file_exists()` som också kan kolla om filer finns, men `is_dir()` är mer specifik för mappar.

När du använder `is_dir()`, kom ihåg:
- Sökvägen kan vara relativ eller absolut.
- Sökvägen måste följa serverns filsystemstruktur, inte URL-strukturen.
- Funktionen returnerar `TRUE` endast om mappen finns och är en katalog.

Säkerhetsaspekter är också viktiga att överväga. Skript som kontrollerar om mappar eller filer finns kan potentiellt avslöja information om serverns filstruktur om de inte hanteras korrekt.

## See Also
- PHP Manual on `is_dir()`: https://www.php.net/manual/en/function.is-dir.php
- PHP Manual on `file_exists()`: https://www.php.net/manual/en/function.file-exists.php
- Stack Overflow discussions on checking for directories in PHP: https://stackoverflow.com/search?q=php+is_dir
