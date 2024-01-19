---
title:                "Kontrollera om en katalog finns"
html_title:           "Bash: Kontrollera om en katalog finns"
simple_title:         "Kontrollera om en katalog finns"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/php/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Vad & varför?
Kontroll av om en katalog finns är en standardfunktion i PHP programmering som används för att säkert verifiera om en specifik katalog redan finns i systemet. Programmerare gör detta för att undvika fel vid skapande eller användning av en katalog, och för att garantera korrekt databehandling.

## Hur man gör:
Här är ett grundläggande exempel på hur du kontrollerar om en katalog finns med PHP:

```PHP
<?php
$dir ="your/directory/path";
if (is_dir($dir)){
    echo "Katalogen finns.";
} else {
    echo "Katalogen finns INTE.";
}
?>
```

Om katalogen finns, kommer det att skriva ut "Katalogen finns." Annars kommer det att skriva ut "Katalogen finns INTE."

## Fördjupning
Det är viktigt att notera att `is_dir()` funktionen introducerades i PHP 4 och har varit en stabil del av språkets standardbibliotek sedan dess. 

Som alternativ kan du också använda `file_exists()` funktionen som kontrollerar både filer och kataloger. Men 'is_dir()' är mer effektiv om du bara behöver verifiera kataloger.

En intressant implementation detalj kring `is_dir()` är att den faktiskt läser innehållet i katalogen för att verifiera dess existens, vilket kan påverka prestanda i stora filsystem.

## Se även
- PHP Dokumentation om 'is_dir()': https://www.php.net/manual/en/function.is-dir.php
- PHP Dokumentation om 'file_exists()': https://www.php.net/manual/en/function.file-exists.php
- För en mer detaljerad förklaring och fler exempel, besök denna diskussion på Stack Overflow: https://stackoverflow.com/questions/18017607/check-dir-if-exists-php