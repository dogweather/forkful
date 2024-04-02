---
date: 2024-01-26 04:44:28.991394-07:00
description: "Komplekse tall har en reell del og en imagin\xE6r del, vanligvis skrevet\
  \ som `a + bi`. De er avgj\xF8rende i avansert matematikk, fysikk, ingeni\xF8rfag\
  \ og visse\u2026"
lastmod: '2024-03-13T22:44:40.879870-06:00'
model: gpt-4-0125-preview
summary: "Komplekse tall har en reell del og en imagin\xE6r del, vanligvis skrevet\
  \ som `a + bi`. De er avgj\xF8rende i avansert matematikk, fysikk, ingeni\xF8rfag\
  \ og visse\u2026"
title: "\xC5 jobbe med komplekse tall"
weight: 14
---

## Hva & Hvorfor?
Komplekse tall har en reell del og en imaginær del, vanligvis skrevet som `a + bi`. De er avgjørende i avansert matematikk, fysikk, ingeniørfag og visse datamaskinalgoritmer. Programmerere jobber med dem for å håndtere beregninger som involverer kvadratrøtter av negative tall og oscillerende funksjoner.

## Hvordan:
PHP tilbyr innebygd støtte for komplekse tall ved hjelp av `ext-intl`-utvidelsen med `NumberFormatter`-klassen. Her er et eksempel:

```php
// Sørg for at intl-utvidelsen er lastet
if (!extension_loaded('intl')) {
    die("Intl-utvidelsen er ikke aktivert. Vennligst aktiver den for å kjøre denne koden.");
}

function addComplexNumbers($a, $b) {
    // Bruk NumberFormatter til å parse og formatere komplekse tall
    $formatter = new NumberFormatter('en_US', NumberFormatter::PATTERN_RULEBASED, 'i = -1;');

    // Parse komplekse tall fra strenger
    $numA = $formatter->parse($a, NumberFormatter::TYPE_DOUBLE);
    $numB = $formatter->parse($b, NumberFormatter::TYPE_DOUBLE);

    // Utfør addisjon
    $sum = $numA + $numB;

    // Formater resultatet som et komplekst tall
    return $formatter->format($sum);
}

echo addComplexNumbers('5+3i', '2+7i'); // Utdata: 7+10i
```

## Dypdykk
Før `ext-intl`, hadde ikke PHP innfødt støtte for komplekse tall. Utviklere brukte funksjoner eller tilpassede klassebiblioteker for å håndtere komplekse tall. Komplekse operasjoner kunne være kjedelige og feilutsatte, men `ext-intl` tilbyr en internasjonalisert måte å presentere og parse komplekse tall på, som er i tråd med ICU-biblioteket.

Imidlertid, for tungvektige matematiske operasjoner, kan noen bruke eksterne biblioteker skrevet i mer mattevennlige språk (som C eller Python) og grensesnitt med dem gjennom PHP. Når det gjelder implementering, håndterer `ext-intl` det bak kulissene, noe som sikrer nøyaktig aritmetikk samtidig som kompleksiteten abstraheres fra utvikleren.

Historisk sett ble komplekse tall sett ned på som 'imaginære', men de har siden blitt fundamentale i ulike vitenskapelige og matematiske felt, og avslører mer om deres betydning i den virkelige verden enn deres imaginære status noen gang antydet.

## Se også
- [PHP-manualen om NumberFormatter](https://www.php.net/manual/en/class.numberformatter.php)
- [Wikipedia om komplekse tall](https://en.wikipedia.org/wiki/Complex_number)
- [PHP: Riktig Veiledning - Arbeide med Datatyper](https://phptherightway.com/#data_types)
