---
title:                "TypeScript: Stor bokstavering av en streng"
programming_language: "TypeScript"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/typescript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor: 

I mange programmeringsspråk, inkludert TypeScript, kan det være nødvendig å konvertere en streng til store bokstaver. Dette kan være nyttig for å opprettholde konsistens i formatering eller for å søke og sammenligne tekst på en mer pålitelig måte. I denne artikkelen skal vi utforske hvordan man kan kapitalisere en streng i TypeScript.

## Hvordan:

Det finnes flere måter å kapitalisere en streng i TypeScript på. La oss se på et eksempel:

```TypeScript
let string = "hei alle sammen!";
console.log(string.toUpperCase());
```

Dette vil resultere i følgende utskrift: "HEI ALLE SAMMEN!" Her bruker vi metoden `toUpperCase()` for å konvertere strengen til store bokstaver.

En annen måte å kapitalisere en streng på er å bruke en `for`-løkke og `toUpperCase()`-metoden på hvert enkelt tegn i strengen. Dette kan være nyttig hvis du ønsker å gjøre andre manipulasjoner på hvert tegn før du konverterer det.

```TypeScript
let string = "hei alle sammen!";
let capitalizedString = "";

for (let i = 0; i < string.length; i++) {
  capitalizedString += string[i].toUpperCase();
}

console.log(capitalizedString);
```

Dette vil resultere i samme utskrift som det første eksempelet.

## Mer om å kapitalisere en streng:

Det er viktig å merke seg at `toUpperCase()`-metoden ikke bare konverterer bokstaver fra små til store, men også fra et ikke-alfabetisk tegn til et alfabetisk tegn. For eksempel, hvis vi har strengen "h3i!", vil `toUpperCase()`-metoden konvertere dette til "H3I!".

En annen metode som kan brukes for å kapitalisere en streng er `toLocaleUpperCase()`. Denne metoden tar hensyn til land-innstillinger og kan være mer nøyaktig for enkelte språk. For eksempel, hvis du har en spansk tekst og ønsker å kapitalisere den etter spansk grammatikk, kan du bruke `toLocaleUpperCase()`-metoden.

## Se også:

- [TypeScript offisiell dokumentasjon om strenger] (https://www.typescriptlang.org/docs/handbook/basic-types.html#string)
- [Metodebibliotek for JavaScript strenger] (https://www.w3schools.com/js/js_string_methods.asp)