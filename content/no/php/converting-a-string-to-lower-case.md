---
title:    "PHP: Konvertere en streng til små bokstaver"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/php/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Hvorfor

Noen ganger når du arbeider med tekster i programmering, vil du kanskje konvertere strenger til små bokstaver. Dette kan være nyttig for å gjøre det enklere å sammenligne tekster eller bare for å gjøre utskriften mer presentabel. Uansett årsak, er det enkelt å konvertere en streng til små bokstaver i PHP.

## Hvordan gjør man det

For å konvertere en streng til små bokstaver i PHP, kan du bruke den innebygde funksjonen `strtolower()`. Denne funksjonen tar en streng som argument og returnerer en ny streng med alle bokstavene konvertert til små bokstaver. Her er et eksempel på bruk av `strtolower()`:

```PHP
$name = "TINA";
echo strtolower($name);
```

Dette vil skrive ut `tina`. Som du kan se, blir alle bokstavene i strengen konvertert til små bokstaver.

Du kan også bruke `strtolower()` til å konvertere alle bokstavene i en streng til store bokstaver. Dette gjøres ved å bruke funksjonen `strtoupper()` i tillegg til `strtolower()`. Her er et eksempel på hvordan du kan gjøre dette:

```PHP
$name = "tina";
echo strtoupper(strtolower($name));
```

Dette vil skrive ut `TINA`. Først blir bokstavene konvertert til små bokstaver ved hjelp av `strtolower()`, og deretter blir de konvertert til store bokstaver ved hjelp av `strtoupper()`.

## Dypdykk

Når du bruker `strtolower()`, er det viktig å huske at det bare vil fungere på bokstaver. Hvis strengen inneholder tall, symboler eller mellomrom, vil de ikke bli endret til små bokstaver. I tillegg, hvis strengen allerede inneholder små bokstaver, vil de forbli uendret.

En annen nyttig funksjon for å manipulere tekst i PHP er `ucfirst()`, som konverterer den første bokstaven i en streng til stor bokstav. Dette er praktisk hvis du for eksempel vil gjøre den første bokstaven i et navn stor.

## Se også

- [PHP String Functions](https://www.php.net/manual/en/ref.strings.php)
- [PHP strtolower() function](https://www.php.net/manual/en/function.strtolower.php)
- [PHP strtoupper() function](https://www.php.net/manual/en/function.strtoupper.php)
- [PHP ucfirst() function](https://www.php.net/manual/en/function.ucfirst.php)