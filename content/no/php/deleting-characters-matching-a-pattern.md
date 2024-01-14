---
title:    "PHP: Sletting av tegn som matcher et mønster"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/php/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Hvorfor

Har du noen gang ønsket å lage et program som fjerner spesifikke tegn fra en tekststreng? Kanskje du ønsker å fjerne alle tallene eller symbolene fra en tekst? Å kunne slette tegn som matcher et mønster kan være nyttig i mange programmeringsscenarioer. Les videre for å lære hvordan du kan gjøre dette i PHP.

## Hvordan

For å slette tegn som matcher et mønster i PHP, kan du bruke funksjonen `preg_replace()`. La oss se på et eksempel hvor vi ønsker å fjerne alle tallene fra en tekststreng:

```PHP
$text = "Jeg har 5 katter og 2 hunder.";
$text = preg_replace("/[0-9]/", "", $text);
echo $text;
```

Output:

```
Jeg har katter og hunder.
```

I dette eksempelet bruker vi et regulært uttrykk, angitt mellom skråstreker, for å matche alle tall (`[0-9]`). Deretter erstatter vi de matchende tegnene med et tomt tegn (`""`). Dette gjør at vi blir sittende igjen med bare tekst uten tall.

Du kan også bruke spesifikke tegn eller symboler i mønsteret for å fjerne dem fra tekststrengen. La oss se på et annet eksempel hvor vi ønsker å fjerne alle spesialtegn fra en e-postadresse:

```PHP
$email = "john@doe.com";
$email = preg_replace("/[!#$%&'*+-\/=?^_`{|}~]/", "", $email);
echo $email;
```

Output:

```
johndoe.com
```

Å bruke funksjonen `preg_replace()` gjør det enkelt å fjerne tegn fra en tekststreng basert på et mønster du definerer.

## Dypdykk

Nå som du vet hvordan du kan bruke `preg_replace()` til å slette tegn som matcher et mønster, kan du utforske flere muligheter med denne funksjonen. Du kan for eksempel kombinere flere mønstre ved hjelp av pipe (`|`), bruke flagg for å gjøre matchingen case-insensitive, og til og med bruke tilbakekallingsfunksjoner for å utføre mer avanserte handlinger på tekststrengen.

Du kan også leke med regulære uttrykk på nettsteder som [Regex101.com](http://regex101.com) for å bli bedre kjent med mønstermatching og testing av koden din.

## Se også

- [PHP manual for preg_replace()](https://www.php.net/manual/en/function.preg-replace.php)
- [Bruk av regulære uttrykk i PHP](https://www.php.net/manual/en/regexp.introduction.php)
- [Regex101.com](http://regex101.com) - et nyttig verktøy for å teste og lære regulære uttrykk