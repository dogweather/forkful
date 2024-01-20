---
title:                "Att skapa en tillfällig fil"
html_title:           "Bash: Att skapa en tillfällig fil"
simple_title:         "Att skapa en tillfällig fil"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/php/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# Skapa temporär fil i PHP

## Vad & Varför?

Att skapa en temporär fil betyder att du skapar en fil som endast används tillfälligt i applikationens livstid, vanligtvis för att lagra stora data på en säker plats. Detta är populärt bland programmerare eftersom det sparar minnet från belastningen av att hålla denna data.

## Hur gör man:

Här är ett exempel på hur du skapar och skriver till en temporär fil i PHP.

```PHP
<?php
// Skapa temporär fil
$temp = tmpfile();

// Skriv data till filen
fwrite($temp, 'Test data');

// Sök tillbaka till filens början
fseek($temp, 0);

// Läs data från filen
echo fread($temp, 1024);

// Stänger filen, förstörs
fclose($temp);
?>
```
Output:
```
Test data
```

## Fördjupning

Att skapa temporära filer i programmering har varit en gemensam praxis sedan tidiga dagar av programmering. I PHP, `tmpfile()` funktionen från version 4 och framåt, skapar en temporär fil i systemets standarddirectory för temporära filer.

Alternativen till att använda `tmpfile()` är `tempnam()` eller `sys_get_temp_dir()`. Den första skapar en fil med ett unikt namn i den angivna directoryn, medan den andra returnerar sökvägen till systemets temporära filkatalog.

Implementationen av `tmpfile()` vill automatiskt ta bort filen när den är stängd. Detta gör att programmerarna kan fokusera på att lösa mer komplexa problem istället för att oroa sig för minneshanteringen.

## Se också

- php.net officiella dokumentation om hantering av filsystemet, specifikt de temporära filfunktionerna [tmpfile()](https://www.php.net/manual/en/function.tmpfile.php), [tempnam()](https://www.php.net/manual/en/function.tempnam.php) och [sys_get_temp_dir()]().