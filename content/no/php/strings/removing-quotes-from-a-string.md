---
date: 2024-01-26 03:40:49.030077-07:00
description: "\xC5 fjerne anf\xF8rselstegn fra en tekststreng i PHP betyr \xE5 strippe\
  \ bort de irriterende doble (`\"`) eller enkle (`'`) anf\xF8rselstegnene som kan\
  \ rotte til\u2026"
lastmod: '2024-03-13T22:44:40.873691-06:00'
model: gpt-4-0125-preview
summary: "\xC5 fjerne anf\xF8rselstegn fra en tekststreng i PHP betyr \xE5 strippe\
  \ bort de irriterende doble (`\"`) eller enkle (`'`) anf\xF8rselstegnene som kan\
  \ rotte til logikken i koden din eller databaseforesp\xF8rslene."
title: "Fjerne anf\xF8rselstegn fra en streng"
weight: 9
---

## Hva & Hvorfor?
Å fjerne anførselstegn fra en tekststreng i PHP betyr å strippe bort de irriterende doble (`"`) eller enkle (`'`) anførselstegnene som kan rotte til logikken i koden din eller databaseforespørslene. Programmerere gjør dette for å rense eller sanere inndata, for å sikre at tekststrenger trygt kan brukes eller lagres.

## Hvordan:
Her er et enkelt eksempel som bruker PHPs innebygde funksjoner:

```php
$quotedString = "'Hello,' she said, \"It's a fine day!\"";
$unquotedString = str_replace(array("'", "\""), '', $quotedString);
echo $unquotedString; // Skriver ut: Hello, she said, Its a fine day!
```

Enkelt, ikke sant? Denne `str_replace()` funksjonen tar en rekke med tegn som skal fjernes fra strengen, inkludert både enkle og doble anførselstegn.

## Dykk dypere
Tilbake i PHPs tidlige dager måtte utviklere være ekstra forsiktige med anførselstegn i tekststrenger, spesielt når de satte inn data i en database. Feilhåndterte anførselstegn kunne føre til SQL-injeksjonsangrep. Der kom magiske anførselstegn inn, en funksjon som auto-escaped inndata. Den ble avskaffet og til slutt fjernet fordi den oppmuntret til dårlige kodingspraksiser og sikkerhetsproblemer.

Nå bruker vi funksjoner som `str_replace()` eller regex med `preg_replace()` for mer avanserte mønstre. Her er et regex-eksempel:

```php
$quotedString = "'Hello,' she said, \"It's a fine day!\"";
$unquotedString = preg_replace('/[\'"]/', '', $quotedString);
echo $unquotedString;
```

For JSON-data kan du bruke `json_encode()` med alternativer som `JSON_UNESCAPED_SLASHES | JSON_UNESCAPED_UNICODE` for å unngå ekstra skråstreker i anførselstegnene dine.

Når du implementerer, tenk på kanttilfeller. Hva om strengen din er ment å ha visse anførselstegn, som dialog i en historie eller tommer i målinger? Kontekst betyr noe, så skreddersy fjerningen av anførselstegn til dataens tiltenkte bruk.

## Se også
- [PHP: str_replace](https://www.php.net/manual/en/function.str-replace.php)
- [PHP: preg_replace](https://www.php.net/manual/en/function.preg-replace.php)
- [PHP: json_encode](https://www.php.net/manual/en/function.json-encode.php)
- [OWASP: Forebygging av SQL-injeksjon](https://owasp.org/www-community/attacks/SQL_Injection)
