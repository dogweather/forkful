---
title:                "Kontrollera om en mapp finns"
html_title:           "PHP: Kontrollera om en mapp finns"
simple_title:         "Kontrollera om en mapp finns"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/php/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Varför

Att känna till hur man kontrollerar om en katalog finns kan vara otroligt användbart för programutvecklare. Det kan bidra till en mer robust och pålitlig kod, då man kan undvika felmeddelanden och hantera potentiella fel på ett bättre sätt.

## Hur man gör

```PHP
if (is_dir('/sökväg/till/katalog')) {
    echo "Katalogen finns!";
} else {
    echo "Katalogen finns inte...";
}
```

Om du försöker testa en katalog som inte finns, kommer du att få utskriften 'Katalogen finns inte...'. Om katalogen däremot finns på den angivna sökvägen, kommer du istället att få utskriften 'Katalogen finns!'.

##deep dive

För att utföra denna kontroll använder vi PHP-funktionen `is_dir()` som tar emot en sökväg som parameter och returnerar en `boolean`-värde beroende på om katalogen finns eller inte.

Det är också viktigt att notera att denna funktion endast fungerar för lokala sökvägar och inte externt via URL: er.

## Se även

- [PHP: is_dir() - Manual](https://www.php.net/manual/en/function.is-dir.php)
- [PHP: Directory Functions - Manual](https://www.php.net/manual/en/ref.dir.php)