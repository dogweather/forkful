---
title:                "Reguliere expressies gebruiken"
date:                  2024-01-28T22:09:50.445044-07:00
model:                 gpt-4-0125-preview
simple_title:         "Reguliere expressies gebruiken"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/php/using-regular-expressions.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
Reguliere expressies (regex) zijn zoekpatronen die worden gebruikt om combinaties van karakters in strings te matchen. Programmeurs gebruiken ze voor taken zoals validatie, zoeken en tekstverwerking, omdat ze krachtig zijn en tijd besparen.

## Hoe te gebruiken:
Om regex in PHP te gebruiken, gebruik je typisch `preg_match` voor het vinden van een match, of `preg_replace` voor zoek-en-vervang. Hier is een snelle blik:

```php
<?php
$string = "De snelle bruine vos springt over de luie hond.";

// Controleer of 'snelle' in de string staat
if (preg_match("/snelle/", $string)) {
  echo "Match gevonden!";
} else {
  echo "Geen match gevonden.";
}
// Uitvoer: Match gevonden!

// Vervang 'bruine' door 'rode'
$vervangenString = preg_replace("/bruine/", "rode", $string);
echo $vervangenString;
// Uitvoer: De snelle rode vos springt over de luie hond.
?>
```

## Diepgaand
Reguliere expressies bestaan al sinds de jaren 1950 en werden uitvoerig geïmplementeerd in Perl, wat veel andere talen, waaronder PHP, beïnvloedde. Alternatieven voor regex in PHP omvatten functies zoals `strpos()` voor het vinden van substrings of `str_replace()` voor het vervangen van tekst. De PCRE (Perl Compatible Regular Expressions) bibliotheek is wat PHP onder de motorkap gebruikt voor regex-functies, en biedt rijke en krachtige mogelijkheden voor patroonmatching.

## Zie Ook
- [PHP Officiële Documentatie over PCRE](https://www.php.net/manual/en/book.pcre.php)
- [Regular-Expressions.info](https://www.regular-expressions.info/) - voor een grondig begrip van regex.
- [Regex101](https://regex101.com/) - voor het testen en debuggen van je regex-patronen.
