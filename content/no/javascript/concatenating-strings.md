---
title:                "Sammenføyning av strenger"
html_title:           "Javascript: Sammenføyning av strenger"
simple_title:         "Sammenføyning av strenger"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/javascript/concatenating-strings.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
 
String-sammenslåing, også kjent som string-konkatenasjon, er en prosess der man kombinerer to eller flere strenger for å lage en enkelt streng. Dette er en viktig teknikk for å kunne samle forskjellige data og informasjon på en effektiv måte innen programmering.

## Hvordan:
 
I Javascript er det flere måter å konkatenere strenger på. Den enkleste metoden er å bruke "+" operatøren til å slå sammen to strenger, for eksempel:

```Javascript
var navn = "Per";
var etternavn = "Hansen";

var fulltNavn = navn + etternavn;

console.log(fulltNavn);
```

Det vil resultere i output "PerHansen". Man kan også bruke metoden "concat()", som tar imot flere argumenter og returnerer en sammenslått streng.

## Dypdykk:
 
String-sammenslåing har vært en grunnleggende funksjon i programmering siden starten av datamaskinens tid. Det har blitt brukt i en rekke forskjellige språk, inkludert det populære språket C. Alternativene til å konkatenere strenger inkluderer å bruke ferdigdefinerte funksjoner og biblioteker, som ofte har bedre ytelse og funksjonalitet enn standard string-konkatenasjon.

I Javascript er implementasjonen av string-konkatenasjon en del av standardbiblioteket, som gir en enkel og effektiv løsning for å håndtere strenger.

## Se også:

- [MDN String.concat()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/concat)
- [CIS 698 - String Concatenation](https://www.cis.upenn.edu/~matuszek/cit594-2012/Pages/03-understanding-strings.html#string-concatenation)