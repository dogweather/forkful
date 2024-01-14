---
title:                "Javascript: Utskrift av feilsøkingsutdata"
programming_language: "Javascript"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/javascript/printing-debug-output.md"
---

{{< edit_this_page >}}

##Hvorfor

Det er alltid en god idé å inkludere debug-utdata når du utvikler programmering. Dette kan hjelpe deg med å identifisere feil og forstå hvordan koden din fungerer. Ved å skrive ut utdata, kan du raskt finne ut hvor koden din går galt og hvordan du kan løse det.

##Slik gjør du det

Det er flere måter å skrive ut debug-utdata på, men den enkleste metoden er å bruke `console.log()` -funksjonen i JavaScript. Du kan legge til `console.log()` hvor som helst i koden din for å skrive ut variabler, meldinger eller andre viktige verdier.

Her er et eksempel på hvordan du kan bruke `console.log()` for å skrive ut en melding og en variabel:

```Javascript
let navn = "Maria";
console.log("Hei, mitt navn er " + navn);
```

Dette vil skrive ut følgende melding i konsollen din:

```
Hei, mitt navn er Maria
```

Du kan også bruke `console.log()` for å inspisere objekter og arrays. Her er et annet eksempel:

```Javascript
let fruits = ["eple", "banan", "jordbær"];
console.log(fruits);
```

Dette vil skrive ut hele `fruits` -arrayen i konsollen din:

```
["eple", "banan", "jordbær"]
```

##Dypere dykk

Nå som du har lært hvordan du skriver ut debug-utdata, kan det være nyttig å vite at det er flere måter å tilpasse og utnytte dette verktøyet på. For eksempel kan du bruke `console.time()` og `console.timeEnd()` for å måle hvor lang tid det tar for en bestemt del av koden din å kjøre. Du kan også bruke `console.table()` for å få en mer visuell og strukturert utskrift av arrays og objekter.

Det er også verdt å merke seg at du kan bruke `console.error()` for å skrive ut feilmeldinger og `console.warn()` for å vise advarsler. Dette kan være veldig nyttig når du jobber med større prosjekter og trenger å identifisere og håndtere feil raskt.

##Se også

- [Document Object Model (DOM)](https://www.w3schools.com/js/js_htmldom.asp)
- [Vanlige feil i JavaScript](https://www.tutorialspoint.com/javascript/javascript_common_errors.htm)
- [Debugging med Chrome DevTools](https://developers.google.com/web/tools/chrome-devtools/javascript/)

Takk for at du leste denne bloggposten om hvordan du kan skrive ut debug-utdata i JavaScript. Vi håper dette vil hjelpe deg med å bli en mer effektiv utvikler og identifisere feil raskere. Lykke til med kodene dine!