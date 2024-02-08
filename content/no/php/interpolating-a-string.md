---
title:                "Interpolering av en streng"
aliases:
- no/php/interpolating-a-string.md
date:                  2024-01-20T17:51:19.034930-07:00
model:                 gpt-4-1106-preview
simple_title:         "Interpolering av en streng"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/php/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Stringinterpolasjon i PHP betyr å plassere variabler direkte i en streng for å bygge den. Vi gjør det fordi det er raskt og leselig når vi vil bytte ut deler av en streng dynamisk.

## Hvordan gjøre det:
Du kan bruke dobbelt anførselstegn eller heredoc-syntaks for å interpolere variabler i PHP. Se eksemplene under:

```php
// Enkel variabelinterpolasjon
$navn = 'Ola';
echo "Hei, $navn!"; // Ut: Hei, Ola!

// Kompleks variabelinterpolasjon med klammer
$artikkel = array('tittel' => 'PHP Interpolasjon', 'år' => 2023);
echo "Les om {$artikkel['tittel']} utgitt i {$artikkel['år']}."; // Ut: Les om PHP Interpolasjon utgitt i 2023.

// Heredoc-syntaks
$frukt = 'apple';
$cat = <<<CAT
When you have an $frukt, you've got a snack.
CAT;
echo $cat; // Ut: When you have an apple, you've got a snack.
```

## Dykk dypere
Før PHP'ens tid, brukte programmeringsspråk som Perl allerede stringinterpolasjon. PHP adopterte dette senere, noe som ga en enklere måte å sette sammen strenger på. 

Alternativer inkluderer bruk av konkatenering (ved å bruke `.`) eller sprintf-funksjonen for mer komplekse situasjoner. Konkatenering kan bli rotete med mange variabler, og sprintf krever at du holder styr på format og variabler. Interpolasjon holder koden renere og lettere å lese.

Interpolasjon fungerer bare med dobbel anførselstegn og heredoc; enkle anførselstegn vil ikke tolke variablene inne i strengen. Dessuten, når du bruker komplekse variabler, som inneholder arrays eller objektegenskaper, må du omslutte dem med krøllparenteser for å sikre korrekt tolkning.

## Se også
- PHP.net på stringer: https://www.php.net/manual/en/language.types.string.php
- PHP The Right Way on Strings: https://phptherightway.com/#strings
- Stack Overflow diskusjoner om stringinterpolasjon: https://stackoverflow.com/questions/tagged/string-interpolation+php
