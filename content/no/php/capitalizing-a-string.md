---
title:                "PHP: Store bokstaver i en streng"
simple_title:         "Store bokstaver i en streng"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/php/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor

I denne blogginnlegget vil vi gå gjennom hvordan du kan kapitalisere en streng i PHP. Dette kan være nyttig i tilfeller der du vil endre hvordan ord eller setninger ser ut, enten det er for å gjøre teksten mer lesbar eller for å følge et spesifikt format.

## Slik gjør du det

For å kapitalisere en streng i PHP, kan du bruke funksjonen `ucfirst()`. Denne funksjonen vil gjøre første bokstav i strengen stor, mens resten av strengen blir gjort til små bokstaver. Her er et eksempel:

```PHP
$streng = "dette er en tekst";
$kapitalisert = ucfirst($streng);
echo $kapitalisert; // Dette er en tekst
```

Som du kan se, blir den første bokstaven i teksten gjort stor, mens resten av teksten forblir i små bokstaver.

For å kapitalisere alle ordene i en streng, kan du bruke funksjonen `ucwords()`. Denne funksjonen gjør første bokstav i hvert ord stor, mens resten av bokstavene forblir små. Her er et eksempel:

```PHP
$streng = "dette er en tekst";
$kapitalisert = ucwords($streng);
echo $kapitalisert; // Dette Er En Tekst
```

I dette tilfellet blir første bokstav i hvert ord gjort stor, mens resten av bokstavene blir gjort små.

## Dypdykk

Det finnes også andre måter å kapitalisere en streng på i PHP, men bruk av `ucfirst()` og `ucwords()` er de mest vanlige metodene. Det er også viktig å merke seg at disse funksjonene tar hensyn til språkinnstillinger, så bokstaver som "æ", "ø" og "å" vil bli behandlet riktig.

Det kan også være nyttig å vite om funksjonen `mb_convert_case()`, som lar deg konvertere strenger til forskjellige typer casing, inkludert stor forbokstav, små bokstaver og omvendt casing. Du kan lese mer om denne funksjonen og hvordan du bruker den ved å sjekke ut PHP-dokumentasjonen [her](https://www.php.net/manual/en/function.mb-convert-case.php).

## Se også

- [PHP dokumentasjon for `ucfirst()`](https://www.php.net/manual/en/function.ucfirst.php)
- [PHP dokumentasjon for `ucwords()`](https://www.php.net/manual/en/function.ucwords.php)
- [PHP dokumentasjon for `mb_convert_case()`](https://www.php.net/manual/en/function.mb-convert-case.php)