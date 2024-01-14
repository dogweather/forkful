---
title:    "PHP: Utvinning av understrenger"
keywords: ["PHP"]
---

{{< edit_this_page >}}

## Hvorfor

Å ekstrahere substringer er en nyttig ferdighet som kan hjelpe deg i mange ulike programmeringsprosjekter. Ved å kunne hente ut deler av en tekststreng, kan du enkelt behandle og manipulere data, og få programmet ditt til å gjøre akkurat det du ønsker.

## Slik gjør du det

Det er flere ulike måter å ekstrahere substringer på i PHP, og her er to av de mest vanlige metodene:

```PHP
// Metode 1: bruker substr() funksjonen 
$string = "Hei, dette er en tekststreng";
$sub = substr($string, 4, 4); // Ekstraherer "dett" som starter på indeks 4 og er 4 tegn lang
echo $sub; // Vil gi output "dett"

// Metode 2: bruker substr_replace() funksjonen
$string = "Dette er en annen tekst";
$sub = substr_replace($string, "min", 6, 2); // Erstatter "er" på indeks 6 med "min"
echo $sub; // Vil gi output "Dette min en annen tekst"
```

Som du kan se, kan du med disse funksjonene definere hvor du vil starte utdraget, og hvor mange tegn du vil ha med.

## Dypdykk

Når du skal ekstrahere substringer, er det viktig å være klar over at indekseringen starter på 0 i PHP. Det betyr at det første tegnet i en tekststreng har indeks 0, og ikke 1 som vi kanskje er vant til.

En annen nyttig funksjon for å ekstrahere substringer er strpos(), som hjelper deg med å finne startposisjonen til et bestemt ord eller et tegn i en tekststreng.

## Se også

- [PHP Manual: substr()](https://www.php.net/manual/en/function.substr.php)
- [PHP Manual: substr_replace()](https://www.php.net/manual/en/function.substr-replace.php)
- [PHP Manual: strpos()](https://www.php.net/manual/en/function.strpos.php)