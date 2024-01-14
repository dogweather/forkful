---
title:                "TypeScript: Å få tak i dagens dato"
programming_language: "TypeScript"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/typescript/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Hvorfor
Det å kunne få tak i dagens dato er et viktig aspekt ved programmering. Det kan benyttes til mange ulike formål, som for eksempel å logge når en handling har funnet sted, eller å vise frem nåværende dato til brukeren. Uansett formål, er det viktig å vite hvordan man kan få tak i dette informasjonen.

## Hvordan
Her vil jeg vise deg hvordan du kan få tak i nåværende dato ved hjelp av TypeScript.

Først må vi importere nødvendige biblioteker for å få tilgang til funksjoner og metoder relatert til datoer. I TypeScript kan vi gjøre dette ved å bruke følgende syntax:

```TypeScript
import { Date } from 'date-fns';
```

Deretter benytter vi `new`-nøkkelordet for å initialisere en ny instans av `Date`-klassen. Dette gjøres som følger:

```TypeScript
const currentDate = new Date();
```

Vi har nå en variabel, `currentDate`, som inneholder dagens dato og tid.

Vil du kun ha tilgang til datoen, kan du gjøre følgende:

```TypeScript
const today = currentDate.getDate();
```

Dette gir deg datoen, for eksempel 10, hvis det er den 10. dagen i måneden vi befinner oss i.

For å få tak i hele datoen, inkludert år og måned, kan du bruke følgende eksempel:

```TypeScript
const fullDate = currentDate.getFullYear() + "-" + (currentDate.getMonth() + 1) + "-" + currentDate.getDate();
```

Dette vil gi deg datoen i formatet "år-måned-dag", for eksempel "2021-09-10".

## Dypdykk
Date-klassen i TypeScript er veldig fleksibel og har mange nyttige metoder for å hente ut ulik informasjon relatert til dato og tid. Her er noen av de mest brukte metodene:

- `getDate()`: returnerer dagen i måneden
- `getMonth()`: returnerer måneden (01 er januar, 02 er februar osv.)
- `getFullYear()`: returnerer året
- `getDay()`: returnerer ukedagen (0 er søndag, 1 er mandag osv.)
- `getTime()`: returnerer tiden i millisekunder siden 1. januar 1970
- `toLocaleDateString()`: returnerer datoen i et leselig format, basert på brukerens lokale innstillinger

Det finnes også mange ulike biblioteker og pakker som kan gjøre det enklere å jobbe med dato og tid i TypeScript. Det kan være lurt å utforske disse for å finne den som passer best for ditt prosjekt.

## Se Også
- [date-fns dokumentasjon](https://date-fns.org/docs/Getting-Started) (engelsk)
- [NPM pakke for dato og tid i TypeScript](https://www.npmjs.com/package/date-time-ts) (engelsk)