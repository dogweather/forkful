---
title:                "PHP: Skriving av tekstfil"
programming_language: "PHP"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/php/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Hvorfor

Å skrive en tekstfil ved hjelp av PHP kan være svært nyttig for å lagre og behandle data på en enkel måte. Ved å bruke PHP kan du enkelt lage og manipulere tekstfiler som kan være nyttige for alt fra logging av data til å lagre brukerinnstillinger.

## Slik gjør du det

For å skrive en tekstfil i PHP trenger du først å åpne en filstrøm ved hjelp av fopen() funksjonen. Deretter kan du bruke fwrite() funksjonen til å skrive ønsket tekst til filen. Til slutt må du huske å lukke filstrømmen ved hjelp av fclose() funksjonen. Under ser du et eksempel på hvordan dette kan gjøres:

```PHP
$file = fopen("tekstfil.txt", "w") or die("Kan ikke åpne filen!");
$txt = "Dette er en tekst som jeg ønsker å skrive til min tekstfil.";
fwrite($file, $txt);
fclose($file);
```

Når du har kjørt koden over, vil du se at det har blitt opprettet en tekstfil med navnet "tekstfil.txt". Om du åpner denne filen vil du se at teksten du skrev inn i variabelen $txt nå ligger lagret der.

## Dypdykk

Det finnes flere parametere du kan bruke når du skriver en tekstfil i PHP, for eksempel for å skrive til forskjellige linjer i filen eller å legge til tekst i en allerede eksisterende fil. Det finnes også flere måter du kan åpne en filstrøm på, avhengig av hva slags informasjon du ønsker å skrive til filen. Det kan være lurt å utforske dette nærmere og lese mer om de ulike mulighetene som finnes.

## Se også

- [PHP fopen() funksjonen](https://www.w3schools.com/php/func_filesystem_fopen.asp)
- [PHP fwrite() funksjonen](https://www.w3schools.com/php/func_filesystem_fwrite.asp)
- [PHP fclose() funksjonen](https://www.w3schools.com/php/func_filesystem_fclose.asp)