---
title:                "Karakters verwijderen die overeenkomen met een patroon"
aliases:
- nl/php/deleting-characters-matching-a-pattern.md
date:                  2024-01-28T21:59:14.918188-07:00
model:                 gpt-4-0125-preview
simple_title:         "Karakters verwijderen die overeenkomen met een patroon"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/php/deleting-characters-matching-a-pattern.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?

Het verwijderen van karakters die overeenkomen met een patroon in PHP gaat over het vinden van specifieke reeksen karakters in strings en deze verwijderen. Programmeurs doen dit om gegevens te schonen, output te formatteren, of strings te manipuleren om aan specifieke criteria te voldoen, zoals het verwijderen van niet-alfanumerieke karakters uit gebruikersinvoer om veiligheidsredenen.

## Hoe doe je dat:

PHP gebruikt de `preg_replace` functie om karakters die overeenkomen met een patroon te verwijderen met behulp van reguliere expressies. Hier is hoe je cijfers uit een string verwijdert:

```PHP
<?php
$text = "Jaar 2023!";
$pattern = '/\d+/'; // Patroon om alle cijfers te vinden
$result = preg_replace($pattern, '', $text);
echo $result; // Geeft uit: Jaar !
?>
```

En hier is hoe je witruimte verwijdert:

```PHP
<?php
$text = "Te   veel      spaties!";
$pattern = '/\s+/'; // Patroon om alle witruimte te vinden
$result = preg_replace($pattern, ' ', $text);
echo $result; // Geeft uit: Te veel spaties!
?>
```

## Diepgaande Verkenning

Het verwijderen van karakters door overeenkomende patronen is niet nieuw. PHP's `preg_replace` functie, die deze functionaliteit mogelijk maakt, gebruikt Perl-compatibele reguliere expressies, een basis van tekstverwerking sinds de opkomst van Perl in de late jaren '80. Alternatieven voor `preg_replace` zijn `str_replace` voor eenvoudige vervangingen en `trim`, `ltrim`, en `rtrim` voor het verwijderen van witruimtes uit strings. Voor meer genuanceerde patroonverwijderingen kan `preg_replace_callback` worden gebruikt voor extra controle tijdens het vervangingsproces.

Het is goed om te weten dat de PREG in `preg_replace` staat voor Perl Regular Expressions, wat aangeeft dat PHP gebruik maakt van Perl's patroonsyntax. Hier is de uitleg:

- `\d` komt overeen met elk cijfer. Het toevoegen van `+` betekent "één of meer" van het voorafgaande element (cijfers, in dit geval).
- `\s` vindt elke witruimte. Net als bij cijfers, richt de `+` na `\s` zich op lange stukken ruimte.

De keuze tussen `preg_replace` en de alternatieven hangt af van wat je doet. Gebruik `preg_replace` voor ingewikkelde patronen en `str_replace` als het gaat om eenvoudige, directe vervangingen.

Onthoud dat verkeerd gebruik van reguliere expressies kan leiden tot inefficiënte code. Benchmark altijd en gebruik reguliere expressies slim.

## Zie Ook

Voor meer over PHP's stringfuncties en patroonmatching:
- [PHP Handleiding — preg_replace](https://www.php.net/manual/en/function.preg-replace.php)
- [PHP Handleiding — Reguliere Expressies (Perl-Compatibel)](https://www.php.net/manual/en/book.pcre.php)
- [PHP Handleiding — str_replace](https://www.php.net/manual/en/function.str-replace.php)
- [PHP Handleiding — Stringfuncties](https://www.php.net/manual/en/ref.strings.php)

Deze links leiden naar de officiële PHP-documentatie waar je je kunt verdiepen in de details van stringmanipulatie en patroonmatching.
