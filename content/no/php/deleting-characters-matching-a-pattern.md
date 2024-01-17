---
title:                "Slette tegn som matcher et mønster"
html_title:           "PHP: Slette tegn som matcher et mønster"
simple_title:         "Slette tegn som matcher et mønster"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/php/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Sletting av tegn som matcher et mønster er en vanlig oppgave for programmerere. Dette innebærer å fjerne bestemte tegn fra en streng, basert på et spesifikt mønster.

Dette kan være nyttig for å rense eller formatere input-data, for eksempel en brukers input i et søkefelt. Det kan også brukes til å filtrere ut uønsket tekst fra en tekstfil eller en database.

## Hvordan:
```PHP 
// Fjern alle tall fra en streng
$streng = "Hei123 verden456!";
$renset_streng = preg_replace("/[0-9]/", "", $streng);
echo $renset_streng; 
// Output: Hei verden!

// Fjern alle mellomrom fra en streng
$streng = "Dette er en streng med mellomrom";
$renset_streng = preg_replace("/\s+/", "", $streng);
echo $renset_streng;
// Output: Detteerentrengmedmellomrom
```
I eksemplene over bruker vi funksjonen `preg_replace()` i PHP for å erstatte tegn som matcher et gitt mønster med tom streng. Mønsteret er angitt ved hjelp av et såkalt regulært uttrykk (regex). Mer informasjon om bruk av regex i PHP finner du i "Se også"-delen nedenfor.

## Dypdykk:
Sletting av tegn basert på et mønster er en vanlig oppgave i programmering og kan løses på ulike måter. Før introduksjonen av funksjonen `preg_replace()` i PHP 5.3, brukte man hovedsakelig funksjonene `str_replace()` og `substr()` for å slette tegn basert på en posisjon i strengen. Disse funksjonene er fortsatt nyttige for enkle oppgaver, men for mer avanserte mønstermatching anbefales det å bruke `preg_replace()`.

En annen alternativ metode som også er vanlig i andre programmeringsspråk, er å bruke en løkke for å iterere gjennom hvert tegn i strengen og slette det hvis det matcher det gitte mønsteret. Dette kan være mer ressurskrevende og mindre effektivt enn å bruke `preg_replace()`.

Når man bruker `preg_replace()`, er det viktig å passe på å riktig formatere det regulære uttrykket for å få ønsket resultat. Det finnes mange ressurser på nettet for å lære om regex og hvordan man bruker det i forskjellige programmeringsspråk.

## Se også:
- [PHP regex cheatsheet](https://www.php.net/manual/en/regexp.reference.cheatsheet.php)
- [W3Schools: PHP preg_replace() function](https://www.w3schools.com/php/func_regex_preg_replace.asp)
- [RegExr: Online regex tester and debugger](https://regexr.com/)