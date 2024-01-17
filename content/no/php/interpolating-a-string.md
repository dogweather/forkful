---
title:                "Interpolering av en streng"
html_title:           "PHP: Interpolering av en streng"
simple_title:         "Interpolering av en streng"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/php/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Interpolasjon av strenger er en vanlig praksis blant PHP-programmerere for å bygge dynamiske meldinger eller strenger ved å kombinere variabler eller uttrykk inn i en eksisterende streng. Dette gjør det enklere å bygge tekst som trenger å endre seg basert på forskjellige situasjoner eller betingelser.

## Hvordan:

```PHP
$navn = "Sara";
$alder = 25;
echo "Hei $navn! Din alder er $alder år gammel."
```

Utskrift: "Hei Sara! Din alder er 25 år gammel."

```PHP
$produkt = "iPhone";
$pris = 9999;
echo "Vil du kjøpe et $produkt for $pris kroner?"
```

Utskrift: "Vil du kjøpe et iPhone for 9999 kroner?"

## Dykk ned:

Interpolasjon av strenger ble introdusert i PHP 5 og er en mer effektiv måte å bygge tekster på i forhold til concatenation (sammenblanding av strenger). Alternativer til interpolasjon inkluderer string formatting funksjoner som sprintf () og printf (). I tilfelle hvor interpolasjon ikke kan brukes, som for å tilbakevise sikre SQL-spørringer, kan string concatenation fortsatt brukes med passende forholdsregler som å bruke escape-sekvenser.

## Se også:

- [PHP Manual on String Interpolation](https://www.php.net/manual/en/language.types.string.php#language.types.string.syntax.double)
- [PHP String Formatting Functions](https://www.php.net/manual/en/ref.strings.php)
- [PHP Security Best Practices](https://www.php.net/manual/en/security.php)