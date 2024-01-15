---
title:                "Konvertere en streng til små bokstaver"
html_title:           "PHP: Konvertere en streng til små bokstaver"
simple_title:         "Konvertere en streng til små bokstaver"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/php/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Hvorfor

Om du noen gang har jobbet med tekstbehandling eller databasebehandling, så har du sikkert støtt på situasjoner hvor det er nødvendig å konvertere en streng til små bokstaver. Dette kan være for å matche data i en database eller for å sammenligne tekststrenger. Uansett årsak, det er en viktig og nyttig funksjon i PHP som kan gjøre livet ditt enklere.

## Hvordan

Konvertering av en streng til små bokstaver i PHP er en enkel og rask prosess. Det kan gjøres ved hjelp av den innebygde funksjonen `strtolower()`.

```PHP
$string = "Dette ER en TEKSTStreng";
echo strtolower($string);
```

Output:
```
dette er en tekststreng
```

Det er viktig å merke seg at den innebygde funksjonen `strtolower()` tar hensyn til språkinnstillinger i PHP-konfigurasjonen din. Dette betyr at den vil korrekt konvertere tegn som er spesifikke for forskjellige språk, som for eksempel æ, ø, å i norsk språk.

Om du ønsker å konvertere en streng til små bokstaver uten å endre originalstrengen, kan du bruke funksjonen `mb_strtolower()`.

```PHP
$string = "Dette ER en TEKSTStreng";
echo mb_strtolower($string);
```

Output:
```
dette er en tekststreng
```

## Deep Dive

Det er viktig å være klar over at konvertering av en streng til små bokstaver i PHP kan føre til uventede resultater om du ikke er forsiktig. Dette er fordi det finnes visse tegn i forskjellige språk som kan endre seg ved konvertering til små bokstaver.

For eksempel vil "SS" i tysk skrift fremdeles være "SS" etter å ha blitt konvertert til små bokstaver, mens i norsk skrift vil "SS" bli "ß". Det er derfor viktig å ha god forståelse for språket du jobber med og å være forsiktig når du konverterer tekststrenger.

En annen ting å merke seg er at konvertering av tekststrenger til små bokstaver er en prosess som bruker serverens ressurser. Om du jobber med store mengder data, kan dette føre til redusert ytelse og belastning på serveren din. Det er derfor alltid lurt å optimalisere koden din og unngå å gjøre unødvendige konverteringer.

## Se også

- [PHP: strtolower()](https://www.php.net/manual/en/function.strtolower.php)
- [PHP: mb_strtolower()](https://www.php.net/manual/en/function.mb-strtolower.php)