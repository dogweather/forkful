---
title:                "Å bruke regulære uttrykk"
html_title:           "PHP: Å bruke regulære uttrykk"
simple_title:         "Å bruke regulære uttrykk"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/php/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Hvorfor

Regular expressions, eller regex, er et kraftig verktøy som kan hjelpe deg med å effektivt behandle tekst og data. De kan brukes til å søke, erstatte og verifisere data, og er spesielt nyttige for å håndtere store mengder tekst og komplekse mønstre. Hvis du jobber med tekstbehandling eller datahåndtering i PHP, kan det å lære å bruke regulære uttrykk være en verdifull ferdighet.

## Hvordan

```PHP
// Søke etter et mønster i en tekststreng
$tekst = "Hei! Mitt navn er Johan.";
if (preg_match("/Johan/", $tekst)) {
  echo "Fant navnet mitt!"; // Dette vil bli skrevet ut
}

// Erstatte tekst basert på et mønster
$tekst = "Jeg skriver kode på HTML og CSS.";
$ny_tekst = preg_replace("/HTML og CSS/", "PHP", $tekst);
echo $ny_tekst; // Dette vil skrive ut "Jeg skriver kode på PHP."

// Verifisere en e-postadresse
$e-post = "johan@example.com";
if (preg_match("/^[a-z0-9._%+-]+@[a-z0-9.-]+\.[a-z]{2,}$/i", $e-post)) {
  echo "Dette er en gyldig e-postadresse."; // Dette vil bli skrevet ut
}
```

De vanligste regex-funksjonene i PHP er `preg_match()`, `preg_replace()` og `preg_match_all()`. Disse funksjonene tar et mønster og en tekststreng som argumenter, og utfører en handling basert på om mønsteret matcher med teksten. Mønstrene er laget med spesielle regulære uttrykk som kan inkludere jokertegn, alternativer, repetisjoner, og mye mer.

## Deep Dive

Det finnes mange ressurser på nettet for å lære mer om bruk av regulære uttrykk i PHP. PHP.net gir en detaljert dokumentasjon over regex-funksjonene med eksempler og beskrivelser. Det finnes også mange nettsteder og fora som tilbyr nyttige tips og triks for å mestre regex. Det kan også være lurt å bruke nettbaserte regex-testere for å teste og validere mønstrene dine før du implementerer dem i koden din. Regulære uttrykk kan være litt vanskelige å lære seg i begynnelsen, men jo mer du øver og bruker dem, jo mer intuitivt vil det bli.

## Se Også

- [PHP.net - Regular Expressions](https://www.php.net/manual/en/book.pcre.php)
- [RegExr](https://regexr.com/)
- [Stack Overflow - Regular Expression questions tagged PHP](https://stackoverflow.com/questions/tagged/php+regex)