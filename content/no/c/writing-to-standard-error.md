---
title:    "C: Å skrive til standardfeil"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/c/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Hvorfor

Å skrive til standard error er en viktig del av å lage programmer i C. Det tillater oss å kommunisere med brukeren og gi verdifulle feilmeldinger som kan hjelpe med å finne og rette feil i koden vår.

## Slik gjør du det

Du kan enkelt skrive til standard error ved hjelp av funksjonen `fprintf`. Denne funksjonen lar deg skrive en formatert melding til en valgt fil, i dette tilfellet standard error. For eksempel:

```C
fprintf(stderr, "En feil har oppstått: %s\n", error_message);
```

I dette eksempelet skriver vi en feilmelding med en variabel verdi til standard error ved hjelp av stedsholderen `%s`. Dette gjør det enklere for oss å gi mer spesifikk informasjon til brukeren om hva som gikk galt.

Det er også viktig å merke seg at standard error ofte brukes sammen med standard output når vi ønsker å gi ut data til brukeren. Vi kan bruke `fprintf` på samme måte, men med `stdout` som filnavn. For eksempel:

```C
fprintf(stdout, "Resultatet er: %d\n", resultat);
```

## Dykk dypere

Å skrive til standard error kan også være nyttig når vi ønsker å logge informasjon eller feilmeldinger i våre programmer. Ved å omdirigere standard error til en fil, kan vi lagre detaljert informasjon om kjøringen av vårt program og bruke det senere for feilsøking eller analyse.

Vi kan også bruke `fprintf` til å skrive til forskjellige filer samtidig, ved å gi flere filnavn som argumenter. Dette kan være spesielt nyttig når vi ønsker å skrive til både standard error og en loggfilsamtidig.

## Se også

- [fprintf function in C](https://www.programiz.com/c-programming/library-function/stdio.h/fprintf) 
- [Standard error in C](https://www.geeksforgeeks.org/standard-error-c/) 
- [Debugging C programs with standard error](https://stackoverflow.com/questions/5146656/debugging-c-programs-with-standard-error)