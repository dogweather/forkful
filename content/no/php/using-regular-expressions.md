---
title:    "PHP: Å bruke regulære uttrykk"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/php/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Hvorfor

Det kan være fristende å bare jobbe med enkle tekstbehandlingsfunksjoner i PHP, men å lære hvordan man bruker regulære uttrykk kan være svært nyttig i mer komplekse situasjoner. Regulære uttrykk er en måte å søke og manipulere tekstbaserte data på, og kan brukes til alt fra validering av skjemafelt til å finne og erstatte tekst i store filer.

## Hvordan

```PHP
$tekst = "Hei! Ønsker du å lære PHP-programmering? Send oss en melding da vel!";
if (preg_match("/PHP/", $tekst)) {
  echo "Fant 'PHP' i teksten!";
} else {
  echo "Fant ikke 'PHP' i teksten.";
}
```

Dette er et enkelt eksempel på hvordan man kan bruke preg_match-funksjonen i PHP til å søke etter et bestemt mønster (i dette tilfellet strengen "PHP") i en tekststreng. Output vil her være "Fant 'PHP' i teksten!", siden teksten inneholder ordet "PHP".

Et annet nyttig preg-funksjon er preg_replace, som lar deg erstatte et mønster med en annen tekst. For å erstatte alle forekomster av "helt" med "svært" i en tekststreng, kan man for eksempel bruke følgende kode:

```PHP
$tekst = "Det er helt fantastisk at du lærer PHP-programmering!";
$nytekst = preg_replace("/helt/", "svært", $tekst);
echo $nytekst;
```

Output vil da være "Det er svært fantastisk at du lærer PHP-programmering!".

## Dypdykk

For å bli virkelig dyktig i å bruke regulære uttrykk, anbefales det å lese dokumentasjonen til PHP for å lære mer om de ulike funksjonene som er tilgjengelige. Det er også nyttig å prøve seg frem med ulike mønstre og se hvordan de fungerer.

En ting som er viktig å være klar over er at regulære uttrykk kan være svært komplekse, og noen ganger er det bedre å bruke mer spesialiserte funksjoner for å utføre oppgaver som f.eks. validering av e-postadresser eller datoer. Det er derfor viktig å vurdere om regulære uttrykk er den beste løsningen for din spesifikke oppgave.

## Se også

- [PHP: Regulære uttrykk](https://www.php.net/manual/en/reference.pcre.pattern.syntax.php)
- [Regular-Expressions.info](https://www.regular-expressions.info/)
- [PHP: Validere e-postadresser med regulære uttrykk](https://www.php.net/manual/en/filter.examples.validation.php)