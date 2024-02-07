---
title:                "Sette stor bokstav i en streng"
date:                  2024-02-03T19:06:01.646920-07:00
model:                 gpt-4-0125-preview
simple_title:         "Sette stor bokstav i en streng"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/php/capitalizing-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å gjøre om en streng til stor forbokstav innebærer å endre det første tegnet i gitt tekst til stor bokstav, for å sikre at setninger, titler eller egennavn starter korrekt i et datasett. Programmerere utfører ofte kapitalisering av strenger for datanormalisering, for å forbedre lesbarheten eller sikre konsistens i brukerinndata eller tekstlig databehandling.

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
