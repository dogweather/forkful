---
title:                "Sette streng til store bokstaver"
date:                  2024-01-19
html_title:           "Arduino: Sette streng til store bokstaver"
simple_title:         "Sette streng til store bokstaver"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/php/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å kapitalisere en streng betyr å gjøre første bokstav i hvert ord til en stor bokstav. Programmerere gjør dette for å forbedre lesbarheten av tekst, som i titler eller navn.

## Slik gjør du:
Kapitaliser en streng med `ucwords()` for hvert ord eller `strtoupper()` for hele strenger. Her er et eksempel:

```php
<?php
$tekst = "hallo verden!";
$kapitalisert = ucwords($tekst);
$fullStor = strtoupper($tekst);

echo $kapitalisert; // Output: Hallo Verden!
echo "\n";
echo $fullStor; // Output: HALLO VERDEN!
?>
```

## Dypdykk:
Tilbake på 80-tallet, i C programmeringsspråket, startet vi å manipulere tekst med funksjoner som `toupper()`. I PHP har vi flere verktøy for dette. `ucfirst()` gjør kun første bokstav i en streng stor, mens `ucwords()` tar seg av hvert ord. Det er også `mb_convert_case()` for multibyte (UTF-8) strenger, viktig for å støtte internasjonale språk korrekt. Hver funksjon bruker litt CPU-tid, så tenk på behovet før du bruker dem på store tekststykker.

## Se Også:
- PHP offisiell dokumentasjon for strengfunksjoner: [PHP: String Functions - Manual](https://www.php.net/manual/en/ref.strings.php)
- Utforsk `mb_convert_case()` for UTF-8 støtte: [PHP: mb_convert_case - Manual](https://www.php.net/manual/en/function.mb-convert-case.php)
- Lær om funksjonen `strtolower()` for å gjøre tekst til små bokstaver: [PHP: strtolower - Manual](https://www.php.net/manual/en/function.strtolower.php)
