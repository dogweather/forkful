---
title:                "PHP: Stor bokstav i en streng"
programming_language: "PHP"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/php/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor

I denne blogginnlegget skal vi se nærmere på hvordan du kan konvertere små bokstaver til store bokstaver i PHP, også kjent som å "capitalize" en streng. Dette er en vanlig oppgave når du jobber med tekstbehandling og formatering av tekst, så det er nyttig å vite hvordan man gjør det i PHP.

## Hvordan

For å gjøre dette i PHP, kan du bruke funksjonen `strtoupper()` som tar et argument (en streng) og returnerer en ny streng med alle bokstavene i store bokstaver. La oss se på et eksempel:

```PHP
$string = "hei alle sammen!";
$capitalized_string = strtoupper($string);
echo $capitalized_string;
```
Output:
`HEI ALLE SAMMEN!`

Som du ser, blir alle bokstavene i strengen forandret til store bokstaver ved hjelp av `strtoupper()` funksjonen.

Hvis du vil at kun den første bokstaven i strengen skal være stor, kan du bruke funksjonen `ucfirst()` i tillegg til `strtoupper()`:

```PHP
$string = "hei alle sammen!";
$capitalized_string = ucfirst(strtolower($string));
echo $capitalized_string;
```
Output:
`Hei alle sammen!`

I dette eksempelet brukte vi også `strtolower()` funksjonen for å gjøre alle bokstavene små før vi brukte `ucfirst()` for å gjøre den første bokstaven stor.

## Deep Dive

Hvis du vil forstå mer om hvordan `strtoupper()` og `ucfirst()` funksjonene fungerer, kan vi ta en titt på det underliggende konseptet - ASCII-kodene til bokstavene. I ASCII-tabellen er det et nummer tilordnet til hver bokstav, og denne verdien avgjør om bokstaven er stor eller liten. For eksempel er "a" representert av tallet 97 og "A" av tallet 65.

Når du bruker `strtoupper()` funksjonen, kan du tenke på det som å "øke" ASCII-koden til hver bokstav med 32, slik at små bokstaver blir til store bokstaver. På samme måte, når du bruker `ucfirst()` funksjonen, øker den ASCII-koden til den første bokstaven i strengen med 32, mens resten av bokstavene forblir uendret.

## Se også

- [PHP string funksjoner](https://www.php.net/manual/en/ref.strings.php)
- [ASCII-kode og ASCII-tabellen](https://www.ascii-code.com/)