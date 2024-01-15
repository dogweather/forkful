---
title:                "Analysere HTML"
html_title:           "Javascript: Analysere HTML"
simple_title:         "Analysere HTML"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/javascript/parsing-html.md"
---

{{< edit_this_page >}}

## Hvorfor

Hvis du jobber med webutvikling eller automatisering av nettinnhold, kan du ofte støte på behovet for å trekke ut spesifikk informasjon fra en HTML-fil. Dette er hvor parsing av HTML kommer inn i bildet. Det er en viktig ferdighet å lære for å gjøre jobben din mer effektiv og for å kunne automatisere mange manuelle oppgaver.

## Hvordan

Parsing av HTML kan gjøres ved hjelp av Javascript. Her er et enkelt eksempel på hvordan du kan trekke ut informasjon fra en HTML-fil ved hjelp av en "getElementsByTagName" kommando.

```Javascript
<script>
  var elementer = document.getElementsByTagName("p");
  for (var i = 0; i < elementer.length; i++) {
    console.log(elementer[i].innerText);
  }
</script>
```

Det vil skrive ut innholdet i alle "p" elementer på siden i konsollen. Dette er bare et grunnleggende eksempel, men ved å lære mer om Javascript og DOM (Document Object Model), kan du utføre mer avanserte parsing-oppdrag.

## Dypdykk

Parsing av HTML innebærer å trekke ut data fra HTML-strukturen og lagre den i en variabel eller gjøre noe annet med den. Det finnes forskjellige metoder og teknikker du kan bruke for å oppnå dette ved hjelp av Javascript.

En annen vanlig metode er å bruke "querySelector()" eller "querySelectorAll()" kommandoer for å hente ut spesifikke elementer med klasser eller ID-er. Du kan også bruke regular expressions for å finne og erstatte bestemte deler av HTML-koden. Det er viktig å ha en god forståelse av HTML-strukturen og hvordan man navigerer i DOM for å kunne effektivt utføre parsing-oppdrag.

## Se også

- [W3Schools - Javascript and HTML DOM](https://www.w3schools.com/js/js_htmldom.asp)
- [MDN - Guide to HTML parsing with Javascript](https://developer.mozilla.org/en-US/docs/Web/Guide/HTML/Using_HTML_parser_from_JavaScript)
- [Stack Overflow - How to parse HTML using Javascript](https://stackoverflow.com/questions/4849434/how-to-parse-the-html-table-and-get-the-data-using-javascript)