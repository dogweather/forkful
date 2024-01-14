---
title:                "TypeScript: Sammenslåing av strenger"
programming_language: "TypeScript"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/typescript/concatenating-strings.md"
---

{{< edit_this_page >}}

## Hvorfor

Å jobbe med string-sammenslåing (concatenating strings) kan være en nyttig ferdighet å ha når man programmerer, spesielt når man jobber med tekstlige data. Ved å kombinere forskjellige strenger kan man enkelt og effektivt lage nye tekststrenger som inneholder informasjon fra flere kilder.

## Slik gjør du det

For å slå sammen strenger i TypeScript kan man bruke operatoren "+" eller metoden ".concat()". Se eksemplene nedenfor:

```TypeScript
let navn = "Sara";
let yrke = "Designer";

// Med operatoren "+"
let beskrivelse = "Hei, jeg heter " + navn + " og jobber som " + yrke + ".";

// Med metoden ".concat()"
let beskrivelse = "Hei, jeg heter ".concat(navn, " og jobber som ", yrke, ".");
```

Når man printer ut variabelen "beskrivelse", vil man få følgende output:

```
Hei, jeg heter Sara og jobber som Designer.
```

Man kan også kombinere strenger og tall ved å konvertere tallet til en string med metoden ".toString()". Se eksempelet nedenfor:

```TypeScript
let antallKatter = 3;

let beskrivelse = "Jeg har " + antallKatter.toString() + " katter.";

console.log(beskrivelse);
```

Output vil da være:

```
Jeg har 3 katter.
```

## Dypdykk

I tillegg til å bruke "+" og ".concat()" til å slå sammen strenger i TypeScript, kan man også bruke template literals (`` ` ``) for å enklere skrive og formatere strenger. Se eksempelet nedenfor:

```TypeScript
let favorittFarge = "blå";

let beskrivelse = `Min favorittfarge er ${favorittFarge}.`;

console.log(beskrivelse);
```

Her vil output være:

```
Min favorittfarge er blå.
```

Template literals gjør det også mulig å inkludere variabler eller uttrykk direkte inne i en streng ved å bruke ${}. Dette kan gjøre koden mer lesbar og enklere å vedlikeholde.

## Se også

- [Dokumentasjon om strings på TypeScript sine offisielle nettsider](https://www.typescriptlang.org/docs/handbook/basic-types.html#string)
- [En guide til template literals](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Template_literals)
- [En tutorial om string-sammenslåing i TypeScript](https://www.pluralsight.com/guides/string-concatenation-in-typescript)