---
title:                "Javascript: Analysering av HTML"
simple_title:         "Analysering av HTML"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/javascript/parsing-html.md"
---

{{< edit_this_page >}}

## Hvorfor

Hvis du noensinne har jobbet med å lage et nettsted, har du sannsynligvis kommet over HTML-kode. HTML (HyperText Markup Language) er selve grunnlaget for å lage nettsider, og det er viktig for å kunne forstå hvordan det fungerer. En måte å gjøre dette på er ved å parse HTML-kode ved hjelp av Javascript. Dette vil tillate deg å manipulere og endre HTML-koden for å skape et dynamisk og interaktivt nettsted.

## Hvordan gjøre det

For å begynne å parse HTML med Javascript, må du først sørge for at du har en HTML-fil og en Javascript-fil som er koblet sammen via en ```<script>``` tag. Deretter kan du bruke DOM (Document Object Model) for å få tilgang til HTML-elementene og manipulere dem.

La oss si at du har følgende HTML-kode:

```html
<html>
  <head>
    <title>Min side</title>
  </head>
  <body>
    <h1>Velkommen!</h1>
    <p>Dette er min nettside.</p>
  </body>
</html>
```

For å få tak i overskriften kan du bruke følgende Javascript-kode:

```Javascript
var header = document.getElementsByTagName("h1")[0];
console.log(header.innerText); // Vil skrive ut "Velkommen!"
```

På samme måte kan du manipulere og endre andre deler av HTML-koden, for eksempel ved å legge til nye elementer eller fjerne eksisterende. Dette gjør at du kan lage et dynamisk nettsted som reagerer på brukerens handlinger.

## Dykk dypere

Når du begynner å parse HTML-kode, er det viktig å huske på at du må bruke riktig syntaks og tilnærming for å få tilgang til de riktige elementene. DOM har en hierarkisk struktur, der hvert element er et "barn" av et annet element. Det er derfor viktig å forstå hvordan dette hierarkiet fungerer for å kunne navigere og manipulere HTML-koden effektivt.

En annen ting å merke seg er at parsing av HTML-kode kan være ressurskrevende, spesielt hvis du har mange komplekse elementer på siden din. Du bør derfor være forsiktig med hvor ofte og hvor mange ganger du parser HTML for å unngå unødvendig belastning på nettstedet ditt.

## Se også

- [W3 Schools - DOM manipulation](https://www.w3schools.com/js/js_htmldom.asp)
- [MDN web docs - Working with APIs](https://developer.mozilla.org/en-US/docs/Learn/JavaScript/Client-side_web_APIs/Manipulating_documents)
- [CSS-Tricks - Manipulating the DOM with Javascript](https://css-tricks.com/manipulating-the-dom-with-javascript/)