---
title:                "PHP: Konvertere en streng til små bokstaver"
programming_language: "PHP"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/php/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Hvorfor

Det er flere grunner til å konvertere en streng til små bokstaver. Det kan være for å sikre ensartethet i en database, for å unngå sammenligningsfeil mellom strenger eller simpelthen for å gjøre tekstbehandling enklere.

## Hvordan gjøre det

For å konvertere en streng til små bokstaver, kan du bruke funksjonen `strtolower ()`. Her er et eksempel på hvordan du kan bruke denne funksjonen:

```PHP
$str = "HELLO WORLD";
echo strtolower($str);
```

Dette vil gi følgende utdata:

```PHP
hello world
```

Som du kan se, er alle bokstavene i strengen nå konvertert til små bokstaver.

## Dypdykk

Når du bruker `strtolower ()` -funksjonen, er det viktig å være klar over at den bare støtter ASCII-tegn. Hvis du trenger å konvertere en streng som inneholder ikke-ASCII-tegn, må du bruke funksjonen `mb_strtolower ()`.

Du bør også være oppmerksom på at hvis du har behov for å konvertere en streng til små bokstaver, men beholde noen bokstaver som store bokstaver (for eksempel initialer i et navn), kan du bruke funksjonen `ucwords ()` på strengen etter å ha konvertert den til små bokstaver.

## Se også

- [strtolower() documentation](https://www.php.net/manual/en/function.strtolower.php)
- [mb_strtolower() documentation](https://www.php.net/manual/en/function.mb-strtolower.php)
- [ucwords() documentation](https://www.php.net/manual/en/function.ucwords.php)