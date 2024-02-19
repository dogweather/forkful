---
aliases:
- /no/php/converting-a-string-to-lower-case/
date: 2024-01-20 17:38:55.294913-07:00
description: "\xC5 konvertere en streng til sm\xE5 bokstaver betyr \xE5 endre alle\
  \ store bokstaver i en tekststreng til tilsvarende sm\xE5 bokstaver. Programmerere\
  \ gj\xF8r dette for\u2026"
lastmod: 2024-02-18 23:08:53.962844
model: gpt-4-1106-preview
summary: "\xC5 konvertere en streng til sm\xE5 bokstaver betyr \xE5 endre alle store\
  \ bokstaver i en tekststreng til tilsvarende sm\xE5 bokstaver. Programmerere gj\xF8\
  r dette for\u2026"
title: "Konvertere en streng til sm\xE5 bokstaver"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å konvertere en streng til små bokstaver betyr å endre alle store bokstaver i en tekststreng til tilsvarende små bokstaver. Programmerere gjør dette for konsistens i datalagring og -søk, slik at sammenligning av strenger ikke påvirkes av bokstavstørrelse.

## Hvordan:

Med PHP kan du bruke `strtolower()` funksjonen for å konvertere en streng til små bokstaver. Slik gjør du det:

```php
<?php
$tekst = "Hallo Norge!";
$små_bokstaver = strtolower($tekst);
echo $små_bokstaver; // "hallo norge!"
?>
```

Sample output:
```
hallo norge!
```

For å konvertere til små bokstaver som respekterer lokaliserte bokstavtyper, som ø, å, og æ, bruker du `mb_strtolower()`:

```php
<?php
$tekst = "BÆR og SØTT";
$små_bokstaver = mb_strtolower($tekst, 'UTF-8');
echo $små_bokstaver; // "bær og søtt"
?>
```

Sample output:
```
bær og søtt
```

## Dykket Dypt:

Før `strtolower()` og `mb_strtolower()`, hadde utviklere ikke innebygd funksjonalitet for å enkelt endre bokstavstørrelse, og måtte kanskje implementere egne løsninger. `mb_strtolower()` ble lagt til for å støtte multibyte-tegnsett, som er viktig for ikke-engelske språk.

Alternativer til `strtolower()` kan inkludere `strtoupper()` for motsatt effekt, eller RegExp-baserte løsninger for mer spesifikke eller komplekse case-transformasjoner. En `ucfirst()` funksjon er også tilgjengelig for å gjøre første bokstav i en streng stor.

Når det gjelder implementasjon, bruker `strtolower()` PHPs interne tegnkart, mens `mb_strtolower()` tillater definisjon av et spesifikt tegnsett, som ofte er nødvendig for å håndtere spesielle bokstaver riktig.

## Se også:

- PHPs offisielle dokumentasjon for `strtolower()`: https://www.php.net/manual/en/function.strtolower.php
- PHPs offisielle dokumentasjon for `mb_strtolower()`: https://www.php.net/manual/en/function.mb-strtolower.php
- PHPs offisielle dokumentasjon for tegnbearbeiding: https://www.php.net/manual/en/book.mbstring.php
