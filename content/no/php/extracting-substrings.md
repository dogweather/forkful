---
title:                "Uthenting av substringer"
html_title:           "PHP: Uthenting av substringer"
simple_title:         "Uthenting av substringer"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/php/extracting-substrings.md"
---

{{< edit_this_page >}}

## Hvorfor

Å trekke ut substrings er en viktig funksjon i PHP, spesielt når man jobber med tekstbehandling. Det lar deg enkelt hente ut deler av tekststrenger og bruke dem til å manipulere eller analysere data. Dette kan være nyttig for å behandle brukerinput, søke i store tekstdokumenter og mye mer. 

## Hvordan gjøre det

Det finnes flere forskjellige funksjoner i PHP som lar deg trekke ut substrings. Her er noen eksempler på hvordan du kan gjøre det: 

```PHP
// Hente ut en del av en tekststreng fra en bestemt posisjon
$tekst = "Dette er en tekststreng";
$del = substr($tekst, 5); // $del vil nå være "er en tekststreng"

// Hente ut en bestemt lengde av en tekststreng fra en gitt posisjon
$tekst = "Dette er en tekststreng";
$del = substr($tekst, 0, 5); // $del vil nå være "Dette"

// Hente ut en del av en tekststreng basert på et spesifikt ord
$tekst = "Dette er en tekststreng";
$del = strstr($tekst, "en"); // $del vil nå være "en tekststreng"
```

Som du kan se, er det enkelt å hente ut ønskede substrings ved å bruke disse funksjonene. Du kan også bruke flere forskjellige parametere for å spesifisere hvor du vil starte og avslutte dine substrings. 

## Dykkertunn

For de som ønsker å utforske mer avanserte funksjoner for å trekke ut substrings, kan visse funksjoner som preg_match og preg_split være nyttige. Disse bruker regulære uttrykk og gir større fleksibilitet når det kommer til å finne og hente ut spesifikke deler av tekst. 

En annen interessant funksjon er str_split, som lar deg splitte en tekststreng opp i mindre deler basert på en gitt lengde. Dette kan være nyttig når du jobber med store mengder tekst og ønsker å dele det opp i mindre biter for enklere behandling. 

## Se Også

- [Offisiell PHP Dokumentasjon for substr](https://www.php.net/manual/en/function.substr.php)
- [Offisiell PHP Dokumentasjon for preg_match](https://www.php.net/manual/en/function.preg-match.php)
- [Offisiell PHP Dokumentasjon for str_split](https://www.php.net/manual/en/function.str-split.php)