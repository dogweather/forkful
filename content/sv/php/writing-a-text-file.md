---
title:                "Skriva en textfil"
html_title:           "Arduino: Skriva en textfil"
simple_title:         "Skriva en textfil"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/php/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att skriva en textfil i PHP innebär att man skapar eller ändrar innehållet i en fil på servern. Programmerare gör detta för att lagra data, hantera loggfiler eller exportera information som kan användas av andra program.

## Så här gör du:
För att skriva text till en fil i PHP, öppna först filen med `fopen()`, använd sedan `fwrite()` för att skriva, och glöm inte att stänga filen med `fclose()`. Här är ett enkelt exempel:

```PHP
<?php
$file = fopen("exempel.txt", "w") or die("Unable to open file!");
$text = "Hej världen!";
fwrite($file, $text);
fclose($file);
?>
```

Detta skapar eller skriver över `exempel.txt` med texten "Hej världen!".

## Djupdykning
Historiskt sett har att skriva till filer varit en grundläggande del av programmering. PHP erbjuder olika funktioner för filhantering förutom `fopen()`, `fwrite()`, och `fclose()`, såsom `file_put_contents()` som är ett förenklat alternativ för att skriva data direkt till filen. När det gäller implementeringsdetaljer bör säkerhetsaspekter som filrättigheter och överväganden kring filsystemet inte ignoreras.

## Se även
- PHP:s officiella dokumentation om filhantering: [php.net/manual/en/book.filesystem.php](https://www.php.net/manual/en/book.filesystem.php)
- W3Schools PHP File tutorial: [w3schools.com/php/php_file.asp](https://www.w3schools.com/php/php_file.asp)
- PHP The Right Way om filhantering: [phptherightway.com/#files](http://www.phptherightway.com/#files)