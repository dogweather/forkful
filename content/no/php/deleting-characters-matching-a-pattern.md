---
title:                "PHP: Sletting av tegn som matcher et mønster"
programming_language: "PHP"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/php/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Hvorfor
 Noen ganger i programmering kan man komme over en situasjon hvor man ønsker å slette visse tegn fra en streng basert på et mønster. Dette kan være nyttig for å rydde opp i data eller gjøre det mer leselig. Nedenfor skal vi se på hvordan man kan gjøre dette ved hjelp av PHP-kode.

## Slik gjør du det
For å starte, definerer vi en variabel med en streng inneholdende et mønster som vi ønsker å slette. Deretter lager vi en while-løkke som går gjennom hver bokstav i strengen og sjekker om den matcher mønsteret. Hvis den gjør det, sletter vi bokstaven ved å bruke PHP's "unset()" funksjon.

```PHP
<?php
$pattern = '/a/';
$text = "Dette er en tekst med noen a'er i.";
$i = 0;

while (isset($text[$i])) {
    if (preg_match($pattern, $text[$i])) {
        unset($text[$i]);
    }
    $i++;
}

echo $text;
// Output: Dette er en tekst med noen 'er i.
?>
```

Som du kan se i koden over, bruker vi PHP's "preg_match()" funksjonen til å sammenligne hver bokstav med mønsteret vårt. Hvis bokstaven matcher, slettes den ved hjelp av "unset()" funksjonen. Til slutt, skriver vi ut den nye strengen uten de slettede tegnene.

Denne metoden kan også brukes til å slette flere ulike tegn ved å definere et mønster med flere bokstaver eller spesialtegn.

## Ned i dybden
Å slette tegn som matcher et gitt mønster kan være svært nyttig, spesielt når man jobber med store datamengder. Det sparer tid og gjør det enklere å lese og analysere data.

I koden over brukte vi PHP's "preg_match()" funksjon, som er en metode for å utføre et mønster-søk i en streng. Den tar inn et mønster og en streng, og returnerer "true" hvis mønsteret finnes i strengen. Deretter brukte vi "unset()" funksjonen for å slette bokstaven som matcher mønsteret. Det finnes også andre metoder for å slette tegn basert på mønster, som for eksempel PHP's "preg_replace()" funksjon.

Det er viktig å merke seg at denne metoden kun sletter tegn fra en streng, den endrer ikke originalstrengen. For å endre originalstrengen må man lagre den nye strengen i en variabel eller bruke PHP's "str_replace()" funksjon.

## Se også
For mer informasjon om hvordan man kan manipulere strenger i PHP, kan du se følgende ressurser:

- [PHP.net - String Functions](https://www.php.net/manual/en/ref.strings.php)
- [W3Schools - PHP String Functions](https://www.w3schools.com/php/php_ref_string.asp)
- [PHP Tutorials - How to Manipulate Strings in PHP](https://www.phptutorials.com/manipulating-strings-in-php/)

Takk for at du leste denne bloggposten! Håper det var nyttig for deg å lære hvordan man sletter tegn basert på et mønster i PHP.