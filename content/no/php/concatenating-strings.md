---
title:                "Sammenslåing av strenger"
html_title:           "PHP: Sammenslåing av strenger"
simple_title:         "Sammenslåing av strenger"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/php/concatenating-strings.md"
---

{{< edit_this_page >}}

## Hvorfor

Det å kombinere strenger, også kjent som string concatenation på engelsk, er en viktig del av å kunne programmere i PHP. Dette lar deg legge sammen flere tekster for å skape nye setninger eller endre eksisterende tekst. Dette er spesielt nyttig når du jobber med dynamiske data som kommer fra databaser eller brukerinput. Ved å kunne kombinere strenger kan du lage mer tilpassede løsninger og gjøre koden din mer fleksibel.

## Hvordan

For å kombinere strenger i PHP, bruker du "." -operatøren. Dette forteller PHP å legge sammen de ulike strengene. La oss se på et enkelt eksempel:

```PHP
$navn = "Kari";
$alder = 30;
$melding = "Hei, mitt navn er " . $navn . " og jeg er " . $alder . " år gammel.";
echo $melding;
```

I dette eksempelet har vi en variabel som inneholder navnet "Kari" og en annen som inneholder alderen "30". Vi bruker deretter "." -operatøren til å kombinere disse to variablene og en fast tekst i den tredje variabelen. Når vi så printer ut den siste variabelen, får vi følgende output:

"Hei, mitt navn er Kari og jeg er 30 år gammel."

Du kan også kombinere så mange strenger som du ønsker ved å bruke flere "." -operatører. For eksempel:

```PHP
$tekst1 = "Denne teksten ";
$tekst2 = "er kombinert ";
$tekst3 = "av flere strenger."
echo $tekst1 . $tekst2 . $tekst3;
```

Outputen vil da være:

"Denne teksten er kombinert av flere strenger."

## Dykk dypere

Når du kombinerer strenger i PHP, er det viktig å huske på at du må inkludere mellomrom og tegn som komma eller punktum for å få en lesevennlig tekst. Dette kan du gjøre ved å inkludere dem direkte i tekststrengen eller ved å legge dem til som egne variabler. For eksempel:

```PHP
$tekst = "Dette er en ";
$middag = "veldig god middag.";
echo $tekst . $middag;
```

Outputen her blir "Dette er en veldig god middag." Hvis du ikke inkluderer mellomrom i tekststrengene, vil du få en tekst som kan være vanskelig å lese og forstå.

Du kan også kombinere variabler av ulike typer, som for eksempel en tekststreng og et tall. PHP vil automatisk konvertere tallet til en tekststreng og legge dem sammen. Du kan også kombinere strenger med booleans eller andre datastrukturer.

## Se også

For mer informasjon om concatenation og andre nyttige PHP-funksjoner, kan du sjekke ut følgende ressurser:

- [PHP - String Operators](https://www.php.net/manual/en/language.operators.string.php)
- [W3Schools - PHP String Concatenation](https://www.w3schools.com/php/php_operators.asp)
- [Codecademy - PHP Concatenation](https://www.codecademy.com/learn/learn-php/modules/learn-php-strings/cheatsheet)