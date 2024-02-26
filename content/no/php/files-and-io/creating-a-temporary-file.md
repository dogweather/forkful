---
date: 2024-01-20 17:40:53.285695-07:00
description: "Midlertidige filer lar oss lagre data midlertidig under kj\xF8ring av\
  \ et program. Programmerere bruker dem for \xE5 h\xE5ndtere store datastr\xF8mmer,\
  \ redusere\u2026"
lastmod: '2024-02-25T18:49:39.078663-07:00'
model: gpt-4-1106-preview
summary: "Midlertidige filer lar oss lagre data midlertidig under kj\xF8ring av et\
  \ program. Programmerere bruker dem for \xE5 h\xE5ndtere store datastr\xF8mmer,\
  \ redusere\u2026"
title: Opprette en midlertidig fil
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Midlertidige filer lar oss lagre data midlertidig under kjøring av et program. Programmerere bruker dem for å håndtere store datastrømmer, redusere minnebruk eller lagre tilstand under lengre prosesser.

## Hvordan:
```PHP
<?php
// Åpner en midlertidig fil
$tempFil = tmpfile();

// Skriver noe data til filen
fwrite($tempFil, 'Hei, verden av midlertidige filer!');

// Søker tilbake til filens start
rewind($tempFil);

// Leser innholdet i den midlertidige filen
echo fread($tempFil, 1024);

// Lukker og sletter den midlertidige filen
fclose($tempFil);
?>
```
Output:
```
Hei, verden av midlertidige filer!
```

## Deep Dive
Før i tiden, da disker var små og RAM var dyrt, var midlertidige filer kritiske for å håndtere store mengder data. De er fortsatt nyttige i mange sammenhenger, som for eksempel når du opplaster filer eller arbeider med datastreaming. `tmpfile()`-funksjonen i PHP oppretter en fil I et systemets temporære mappe, noe som reduserer risikoen for konflikter og sikkerhetsproblemer. Alternativet `tempnam()` kan brukes for å spesifisere et navn og en plassering for en midlertidig fil direkte, men med dette kommer ytterligere ansvar for å håndtere livssyklusen til filen.

## Se Også
- PHP.net Manual on `tmpfile()`: https://www.php.net/manual/en/function.tmpfile.php
- PHP.net Manual on `tempnam()`: https://www.php.net/manual/en/function.tempnam.php
- PHP.net Manual on Filesystem Functions: https://www.php.net/manual/en/ref.filesystem.php
