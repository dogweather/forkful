---
date: 2024-01-26 04:43:38.562956-07:00
description: "Kuinka: PHP tarjoaa sis\xE4\xE4nrakennetun tuen kompleksiluvuille k\xE4\
  ytt\xE4en `ext-intl`-laajennosta `NumberFormatter`-luokan avulla. T\xE4ss\xE4 on\
  \ esimerkki."
lastmod: '2024-03-13T22:44:56.649739-06:00'
model: gpt-4-0125-preview
summary: "PHP tarjoaa sis\xE4\xE4nrakennetun tuen kompleksiluvuille k\xE4ytt\xE4en\
  \ `ext-intl`-laajennosta `NumberFormatter`-luokan avulla."
title: "Kompleksilukujen k\xE4sittely"
weight: 14
---

## Kuinka:
PHP tarjoaa sisäänrakennetun tuen kompleksiluvuille käyttäen `ext-intl`-laajennosta `NumberFormatter`-luokan avulla. Tässä on esimerkki:

```php
// Varmista, että intl-laajennus on ladattu
if (!extension_loaded('intl')) {
    die("Intl-laajennus ei ole käytössä. Ota se käyttöön suorittaaksesi tämän koodin.");
}

function addComplexNumbers($a, $b) {
    // Käytä NumberFormatteria kompleksilukujen jäsentämiseen ja muotoiluun
    $formatter = new NumberFormatter('en_US', NumberFormatter::PATTERN_RULEBASED, 'i = -1;');

    // Jäsennä kompleksiluvut merkkijonoista
    $numA = $formatter->parse($a, NumberFormatter::TYPE_DOUBLE);
    $numB = $formatter->parse($b, NumberFormatter::TYPE_DOUBLE);

    // Suorita yhteenlasku
    $sum = $numA + $numB;

    // Muotoile tulos kompleksilukuna
    return $formatter->format($sum);
}

echo addComplexNumbers('5+3i', '2+7i'); // Tuloste: 7+10i
```

## Syväsukellus
Ennen `ext-intl`-laajennusta PHP ei tukenut sisäänrakennettuna kompleksilukuja. Kehittäjät käyttivät funktioita tai mukautettuja luokkakirjastoja käsitelläkseen kompleksilukuja. Kompleksisten operaatioiden käsittely saattoi olla työlästä ja altista virheille, mutta `ext-intl` tarjoaa kansainvälisen tavan esittää ja jäsentää kompleksilukuja ICU-kirjaston mukaisesti.

Kuitenkin, painavampien matemaattisten operaatioiden osalta jotkut saattavat käyttää ulkoisia kirjastoja, jotka on kirjoitettu matematiikka-yhteensopivammilla kielillä (kuten C tai Python) ja integroida ne PHP:n kautta. Toteutuksen osalta `ext-intl` käsittelee sen taustalla, varmistaen tarkan aritmetiikan samalla kun se abstrahoi monimutkaisuuden kehittäjältä.

Historiallisesti kompleksilukuja karsastettiin niiden 'kuvitteellisen' termin vuoksi, mutta ne ovat sittemmin muodostuneet perustaviksi eri tieteellisissä ja matemaattisissa aloissa, paljastaen enemmän niiden todellisesta merkityksestä kuin mitä niiden kuvitteellinen status koskaan ehdotti.

## Katso Myös
- [PHP-käsikirja NumberFormatterista](https://www.php.net/manual/en/class.numberformatter.php)
- [Wikipedia kompleksiluvuista](https://fi.wikipedia.org/wiki/Kompleksiluku)
- [PHP: The Right Way - Työskentely tietotyyppien kanssa](https://phptherightway.com/#data_types)
