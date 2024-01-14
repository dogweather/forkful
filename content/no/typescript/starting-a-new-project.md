---
title:    "TypeScript: Å starte et nytt prosjekt"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

## Hvorfor

Å starte et nytt program er en spennende og givende opplevelse for enhver programmerer. Det er en mulighet til å utforske nye ideer, utvide kunnskap og forbedre ferdigheter. Uansett om du er en erfaren utvikler eller nybegynner, kan å starte et nytt prosjekt gi deg muligheten til å skape noe unikt og nyttig.

## Hvordan

Når du skal starte et nytt TypeScript-prosjekt, er det viktig å følge noen grunnleggende trinn. Først må du sørge for at du har installert Node.js, som er nødvendig for å kjøre TypeScript-koden din. Deretter kan du følge disse trinnene:

```TypeScript
// Opprett en ny mappe for prosjektet ditt
mkdir prosjektnavn

// Gå inn i mappen
cd prosjektnavn

// Initialiser NPM og legg til TypeScript som et avhengighetsverktøy
npm init -y
npm install typescript --save-dev

// Opprett en TypeScript-konfigurasjonsfil
npx tsc --init

// Opprett en JavaScript-kildefil og en TypeScript-kildefil
touch index.js
touch index.ts
```

Nå er du klar til å kode ditt første TypeScript-prosjekt! Du kan skrive din TypeScript-kode i index.ts filen og deretter bruke kommandoen `npm run build` for å konvertere koden til JavaScript. Den kompilerte koden vil bli lagret i index.js filen.

En enkel Hello World-applikasjon i TypeScript kan se ut som dette:

```TypeScript
let navn: string = "Verden";
console.log(`Hei ${navn}!`);
```

Når du kjører kommandoen `npm run build`, vil konsollen vise `Hei Verden!` som output.

## Dypdykk

Å starte et nytt TypeScript-prosjekt gir deg også muligheten til å oppdage de mange fantastiske funksjonene som språket har å tilby. Et av de mest spennende aspektene ved TypeScript er muligheten til å håndtere typer. Ved å definere typer for variabler og funksjoner kan du unngå potensielle feil og gjøre koden din mer robust.

I tillegg til typer, tilbyr TypeScript også et bredt utvalg av innebygde metoder og funksjoner som kan hjelpe deg med å effektivisere kodingen din. Fra å håndtere asynkrone oppgaver til å arbeide med lister og objekter, er det mye å oppdage når det kommer til TypeScript.

## Se Også

* [Offisiell TypeScript dokumentasjon](https://www.typescriptlang.org/docs/home.html)
* [Kodeeksempler på GitHub](https://github.com/Microsoft/TypeScript-Samples)
* [TypeScript-prosjekter på npm](https://www.npmjs.com/search?q=keywords:typescript)