---
title:                "Søking og erstatning av tekst"
html_title:           "Lua: Søking og erstatning av tekst"
simple_title:         "Søking og erstatning av tekst"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/php/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Søke- og erstattefunksjon er prosessen der man identifiserer bestemte strenge (tekst) i kildekoden og erstatter den med en annen streng. Programmerere gjør dette primært for å oppdatere, forbedre eller debugge kode raskt.

## Hvordan:

I PHP brukes oftest `str_replace` for å søke og erstatte tekst. Se hvordan i kodeeksemplet nedenfor:

```PHP
<?php
// Definerer originalteksten
$originalTekst = "Hei, Verden!";
// Bytt ut "Verden" med "Norge"
$endretTekst = str_replace("Verden", "Norge", $originalTekst);
// Skriver ut den endrede teksten
echo $endretTekst;
?>
```

Når du kjører koden, vil utdataene være:

```
Hei, Norge!
```

## Dyp Dykk

Historisk sett oppstod søk- og erstattefunksjon med tekstbehandling og programmering, og PHP hadde et rikt utvalg av metoder. Foruten `str_replace`, vi har `preg_replace` og `str_ireplace`.

`preg_replace` er kraftigere og bruker vanlige uttrykk, men er også langsommere. `str_ireplace` er en case-insensitiv versjon av `str_replace`.

Apropos implementeringsdetaljer, husk at `str_replace` er en ikke-destruktiv funksjon: den returnerer en ny streng uten å endre den originale strengen.

## Se Også

For mer om tekstsøk og -erstatning i PHP, se:

- [PHP: str_replace - Manual](https://www.php.net/manual/en/function.str-replace.php)
- [PHP: preg_replace - Manual](https://www.php.net/manual/en/function.preg-replace.php)
- [PHP : str_ireplace - Manual](https://www.php.net/manual/en/function.str-ireplace.php)