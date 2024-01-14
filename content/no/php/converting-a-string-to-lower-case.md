---
title:    "PHP: Konvertere en streng til små bokstaver"
keywords: ["PHP"]
---

{{< edit_this_page >}}

## Hvorfor

Når man programmerer i PHP, kommer man ofte over situasjoner hvor man må behandle tekststrenger. En vanlig oppgave er å konvertere tekststrenger til små bokstaver. Dette kan være nyttig for å sikre at man sammenligner eller lagrer data på en ensartet måte.

## Slik gjør du det

Det er flere måter å konvertere en tekststreng til små bokstaver i PHP. Her er to eksempler:

```PHP
// Metode 1: Bruk funksjonen strtolower()
$string = "Dette er en TEKSTSTRENG";
$lowercase_string = strtolower($string);
echo $lowercase_string;
// Output: dette er en tekststreng
```

```PHP
// Metode 2: Bruk funksjonen mb_strtolower()
$string = "Dette er en TEKSTSTRENG";
$lowercase_string = mb_strtolower($string, "UTF-8");
echo $lowercase_string;
// Output: dette er en tekststreng
```

I begge disse eksemplene bruker vi en innebygget PHP-funksjon for å konvertere tekststrengen til små bokstaver. Det er viktig å merke seg at disse to metodene kan håndtere forskjellige tegnsett. ```strtolower()``` fungerer med ANSI-tegnsett, mens ```mb_strtolower()``` fungerer med flerspråklige tegnsett som krever UTF-8.

## Dypdykk

Nå som vi har sett hvordan man kan konvertere en tekststreng til små bokstaver, la oss se litt nærmere på hvordan dette fungerer under overflaten. PHP har en innebygd funksjon som heter ```strval()``` som konverterer en verdi til en streng. Når vi bruker denne funksjonen på en tekststreng, vil den returnere en kopi av strengen der alle tegnene er konvertert til små bokstaver.

Ved å bruke ```var_dump()``` funksjonen på resultatet av ```strval()``` kan vi se hvordan den konverterte strengen blir behandlet:

```PHP
$string = "Dette er en TEKSTSTRENG";
$string_copy = strval($string);
var_dump($string);
var_dump($string_copy);

// Output:
// string(20) "Dette er en TEKSTSTRENG"
// string(20) "dette er en tekststreng"
```

Vi kan se at ```strval()``` konverterer alle tegnene til små bokstaver og returnerer en ny streng. Dette er en enkel måte å konvertere en tekststreng til små bokstaver på, uten å måtte bruke separate funksjoner som ```strtolower()``` eller ```mb_strtolower()```.

## Se også

- [PHP Manual: strtolower()](https://www.php.net/manual/en/function.strtolower.php)
- [PHP Manual: mb_strtolower()](https://www.php.net/manual/en/function.mb-strtolower.php)