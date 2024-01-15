---
title:                "Å finne lengden til en tekststreng"
html_title:           "PHP: Å finne lengden til en tekststreng"
simple_title:         "Å finne lengden til en tekststreng"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/php/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor

Å finne lengden på en streng er en vanlig oppgave når man jobber med PHP-programmering. Det er nyttig å kunne for å kunne håndtere og manipulere tekstdata på en effektiv måte i koden din.

## Slik gjør du det

Det finnes flere måter å finne lengden på en streng i PHP, avhengig av hva som passer best for oppgaven din. Her er noen eksempler:

```PHP
// Bruker funksjonen strlen() for å finne lengden på en streng
$string = "Dette er en test";
echo strlen($string); // Output blir 16, siden det er 16 tegn i strengen

// Bruker count() funksjonen på en streng som et array av tegn
echo count(str_split($string)); // Output blir også 16

// Bruker mb_strlen() funksjonen for å håndtere flerspråklige strenger
// Denne funksjonen tar hensyn til spesielle tegn og bokstaver som kan oppta mer enn én byte
$string = "Dette er en test på norsk: Værsågod";
echo mb_strlen($string, 'UTF-8'); // Output blir 30

// Du kan også bruke en løkke til å telle hvert tegn i strengen manuelt
$count = 0;
foreach(str_split($string) as $char){
    $count++;
}
echo $count; // Output blir også 30
```

## Dykk dypere

Det er viktig å være klar over at lengden på en streng i PHP er basert på antall tegn, ikke antall ord. Dette betyr at mellomrom også blir telt som et tegn.

Du bør også være oppmerksom på hvilken type tegnkoding som brukes i strengen din. Hvis det er forskjellig fra standarden UTF-8, må du sørge for å sette riktig tegnkoding i parameteren for mb_strlen() funksjonen.

En annen nyttig funksjon for å håndtere strenger er substr() funksjonen, som lar deg hente en del av en streng basert på startposisjon og lengde.

## Se også

- Offisiell PHP Manual for strlen(): https://www.php.net/manual/en/function.strlen.php
- Eksempler på bruk av mb_strlen(): https://www.php.net/manual/en/function.mb-strlen.php
- Dokumentasjon for substr() funksjonen: https://www.php.net/manual/en/function.substr.php