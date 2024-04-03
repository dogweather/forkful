---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:06:01.646920-07:00
description: "\xC5 gj\xF8re om en streng til stor forbokstav inneb\xE6rer \xE5 endre\
  \ det f\xF8rste tegnet i gitt tekst til stor bokstav, for \xE5 sikre at setninger,\
  \ titler eller\u2026"
lastmod: '2024-03-13T22:44:40.868805-06:00'
model: gpt-4-0125-preview
summary: "\xC5 gj\xF8re om en streng til stor forbokstav inneb\xE6rer \xE5 endre det\
  \ f\xF8rste tegnet i gitt tekst til stor bokstav, for \xE5 sikre at setninger, titler\
  \ eller egennavn starter korrekt i et datasett."
title: Sette stor bokstav i en streng
weight: 2
---

## Hvordan:
PHP støtter natively forskjellige funksjoner for å kapitalisere strenger, hver med et forskjellig formål. Her er hvordan du kan bruke dem:

### Kapitalisere den første bokstaven i en streng:
```php
$string = "hello, world!";
$capitalizedString = ucfirst($string);
echo $capitalizedString; // Resultat: Hello, world!
```

### Kapitalisere den første bokstaven i hvert ord:
```php
$string = "hello, world!";
$capitalizedWords = ucwords($string);
echo $capitalizedWords; // Resultat: Hello, World!
```

### Konvertere hele strengen til store bokstaver:
```php
$string = "hello, world!";
$upperCaseString = strtoupper($string);
echo $upperCaseString; // Resultat: HELLO, WORLD!
```

For scenarioer som krever mer tilpasning eller tredjeparts løsninger, kan biblioteker som `mbstring` (for flerbyte strenger) benyttes, spesielt når man håndterer internasjonalisering hvor tegn kan strekke seg utover det grunnleggende ASCII-settet.

### Bruke mbstring for å kapitalisere UTF-8 strenger:
Sørg for at du har `mbstring`-utvidelsen aktivert i din PHP-konfigurasjon, deretter:

```php
$string = "élégant";
$capitalizedString = mb_convert_case($string, MB_CASE_TITLE, "UTF-8");
echo $capitalizedString; // Resultat: Élégant
```

Denne tilnærmingen hjelper til med å nøyaktig kapitalisere strenger som inkluderer ikke-ASCII-tegn, og følger dermed de språklige nyansene i forskjellige språk.
