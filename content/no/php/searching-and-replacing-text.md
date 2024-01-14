---
title:    "PHP: Søke og erstatte tekst"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/php/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Hvorfor

Å søke og erstatte tekst er en viktig funksjon i PHP-programmering. Det lar deg enkelt bytte ut spesifikke ord eller uttrykk i en tekststreng og gjøre store endringer i en kodebase med bare noen få linjer med kode.

## Hvordan

For å søke og erstatte tekst i PHP, bruker vi funksjonen `str_replace()`. Denne funksjonen tar tre argumenter: søketekst, erstatningstekst og tekststrengen som skal søkes i.

```PHP
$text = "Hei alle sammen!";
$ny_text = str_replace("Hei", "Hallo", $text);

echo $ny_text;
```
Output: `Hallo alle sammen!`

Her erstatter vi alle forekomster av ordet "Hei" med "Hallo" i teksten vår.

Vi kan også bruke `str_replace()` til å søke etter flere ord samtidig. Vi bare passer på å angi hver enkelt søketekst og erstatningstekst i separate arrayer. Se eksempelet nedenfor:

```PHP
$text = "Jeg heter Ingrid og jeg elsker å kode";
$søk = array("Ingrid", "elsker", "kode");
$erstatt = array("Maria", "likte", "programmering");

$ny_text = str_replace($søk, $erstatt, $text);

echo $ny_text;
```

Output: `Jeg heter Maria og jeg likte å programmere`

## Dypdykk

I tillegg til å søke og erstatte tekst, kan `str_replace()` også brukes til å fjerne bestemte tegn eller tegnsett fra en tekststreng. Dette gjøres ved å bare utelate erstatningsteksten i funksjonskallet.

```PHP
$text = "Hvordan går det? Jeg trenger å fjerne alle spørsmålstegn fra denne teksten.";

$ny_text = str_replace("?", "", $text);

echo $ny_text;
```

Output: `Hvordan går det Jeg trenger å fjerne alle spørsmålstegn fra denne teksten`

En annen nyttig funksjon som er tilgjengelig for å søke og erstatte tekst i PHP er `str_ireplace()`. Den fungerer på samme måte som `str_replace()`, men den skiller ikke mellom store og små bokstaver i søketeksten. Dette betyr at den vil bytte ut ord uavhengig av hvordan de er skrevet.

## Se også

- PHP.net - [str_replace()](https://www.php.net/manual/en/function.str-replace.php)
- Tutorials Point - [PHP String Replacement](https://www.tutorialspoint.com/php/php_string_replacement.htm)
- W3Schools - [PHP str_ireplace() Function](https://www.w3schools.com/php/func_string_str_ireplace.asp)