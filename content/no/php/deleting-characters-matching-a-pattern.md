---
title:                "PHP: Sletting av tegn som samsvarer med et mønster."
simple_title:         "Sletting av tegn som samsvarer med et mønster."
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/php/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Hvorfor

Noen ganger i programmering må vi fjerne spesifikke tegn som følger et bestemt mønster. Dette kan være for å rydde opp i data eller for å utføre en bestemt operasjon. I denne bloggposten vil vi utforske hvordan man kan slette tegn som passer en bestemt mønster i PHP.

## Hvordan

For å slette tegn som følger et bestemt mønster, kan vi bruke funksjonen `preg_replace()` i PHP. Dette lar oss spesifisere et mønster ved hjelp av et regulært uttrykk, og erstatte alle forekomster av mønsteret med et spesifisert tegn eller streng. La oss se på et eksempel:

```
<?php
$tekst = 'Jeg liker å spise epler og bananer.';
$ny_tekst = preg_replace('/e/l', '4',$tekst);
echo $ny_tekst;
?>
```

Å kjøre dette vil gi følgende output: "Jeg lik4r å spis4 appl4r og banan4r." Her har vi brukt regulært uttrykk "/e/l" for å spesifisere at vi ønsker å erstatte alle forekomster av bokstaven "e" med tallet 4.

Dette er bare ett eksempel på hvordan man kan bruke `preg_replace()` for å slette tegn som passer et mønster. Det finnes mange forskjellige muligheter og kombinasjoner av mønstre og erstatninger som man kan bruke.

## Dypere innblikk

I tillegg til å bruke `preg_replace()` kan man også bruke funksjonene `preg_match()` og `preg_match_all()` for å finne og manipulere tegn som følger et mønster i en gitt tekst.

Den første funksjonen, `preg_match()`, lar oss finne den første forekomsten av et mønster i en tekst, mens den andre, `preg_match_all()`, lar oss finne alle forekomster og lagre dem i en array.

Det finnes også flere såkalte "metategn" som kan brukes i regulære uttrykk for å finne bestemte mønstre. Disse inkluderer blant annet "w" for å finne alle bokstaver, "d" for å finne alle tall, og "s" for å finne alle mellomrom.

Å kunne håndtere og manipulere tekst basert på bestemte mønstre er en viktig del av programmering, og det er derfor viktig å ha god forståelse for hvordan man kan bruke regulære uttrykk i PHP.

## Se også

- [PHP's preg_replace() function](https://www.php.net/manual/en/function.preg-replace.php)
- [Regular Expression Tutorial](https://www.regular-expressions.info/)
- [PHP Regex Cheat Sheet](https://www.phpcheatsheets.com/2019/10/php-regex-cheat-sheet.html)