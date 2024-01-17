---
title:                "Sammensetting av strenger"
html_title:           "PHP: Sammensetting av strenger"
simple_title:         "Sammensetting av strenger"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/php/concatenating-strings.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Concatenating strings i programmering er når du kombinerer to eller flere tekststrenger til en enkelt streng. Dette kan være nyttig for å bygge dynamiske setninger eller lenker. Programmere gjør dette for å lage mer fleksible og dynamiske programmer som kan tilpasse seg ulike situasjoner.

## Hvordan:
For å konkatere strenger i PHP bruker du punktum operatøren ```.```. Se følgende eksempel:

```PHP
$string1 = "Hei, ";
$string2 = "verden!";
echo $string1 . $string2; // output: Hei, verden!
```

Du kan også konkatere strenger ved hjelp av attributter. Se følgende eksempel:

```PHP
$string1 = "Jeg liker ";
$string2 = "å kode på ";
$string3 = "PHP";
echo $string1 .= $string2 .= $string3; // output: Jeg liker å kode på PHP
```

## Dypdykk:
Historisk sett har concatenation vært brukt i programmering for å bygge dynamiske HTML-sider og SQL-uttrykk. Alternativene til string concatenation inkluderer string interpolation og string formatting. Implementeringsdetaljene for string concatenation avhenger av programmeringsspråket som brukes, men det samme konseptet kan bli funnet i de fleste språk.

## Se også:
- [PHP strings dokumentasjon](https://www.php.net/manual/en/language.types.string.php)
- [Different ways to concatenate strings in PHP](https://www.tutorialrepublic.com/faq/how-to-concatenate-strings-in-php.php)
- [String concatenation in other programming languages](https://www.techopedia.com/definition/29912/string-concatenation)