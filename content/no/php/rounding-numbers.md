---
title:                "Avrunding av tall"
date:                  2024-01-26T03:45:46.443122-07:00
model:                 gpt-4-0125-preview
simple_title:         "Avrunding av tall"
programming_language: "PHP"
category:             "PHP"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/php/rounding-numbers.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Avrunding av tall betyr å kutte av desimalene til en bestemt nøyaktighet, ofte til hele tall. Programmerere runder av for å forenkle beregninger, forbedre ytelse, eller gjøre utdata brukervennlige.

## Hvordan:
PHP tilbyr noen måter å avrunde tall på: `round()`, `ceil()`, og `floor()`. Slik fungerer de:

```php
echo round(3.14159);   // Returnerer 3
echo round(3.14159, 2); // Returnerer 3.14

echo ceil(3.14159);    // Returnerer 4, runder alltid opp

echo floor(3.14159);   // Returnerer 3, runder alltid ned
```

## Dypdykk
Avrunding av tall har vært essensielt i matematikk og databehandling siden antikken for å håndtere upraktiske uendelige desimaler. I PHP kan `round()` ta en nøyaktighetsparameter og modus, som påvirker oppførselen - `PHP_ROUND_HALF_UP`, `PHP_ROUND_HALF_DOWN`, etc., definerer hvordan det vil oppføre seg når det møter et ".5" scenario. Nøyaktighet er nøkkelen i finansielle applikasjoner hvor avrunding kan være juridisk regulert, noe som påvirker hvordan `round()` implementeres i kode.

Alternativer til innebygde funksjoner inkluderer egendefinerte avrundingsmetoder eller BC Math-funksjoner for vilkårlig nøyaktighet aritmetikk, som er nyttige for scenarioer som trenger mer kontroll eller har å gjøre med veldig store tall hvor innfødt nøyaktighet kan feile.

## Se Også
Utforsk mer i PHP-manualen:
- [PHP `round` funksjon](https://php.net/manual/en/function.round.php)
- [PHP `ceil` funksjon](https://php.net/manual/en/function.ceil.php)
- [PHP `floor` funksjon](https://php.net/manual/en/function.floor.php)
- [BC Math for vilkårlig nøyaktighet aritmetikk](https://php.net/manual/en/book.bc.php)
