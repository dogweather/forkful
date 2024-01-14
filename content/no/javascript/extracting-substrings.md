---
title:    "Javascript: Utekstrahering av delstrenger"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

## Hvorfor

Å trekke ut substrings kan være en nyttig ferdighet å ha i Javascript-programmering. Det lar deg enkelt hente ut deler av en tekststreng som du trenger, og kan være til nytte for å manipulere data eller generere dynamisk HTML-innhold.

## Hvordan gjøre det

For å trekke ut en substring i Javascript, kan du bruke metoden `substring()` eller `substr()`. Begge disse metodene tar inn to parametere: startindeksen og lengden på substringen. La oss se på et eksempel med en tekststreng som inneholder et navn:

```Javascript
let navn = "Kari Nordmann";
let etternavn = navn.substring(5,12); // "Nordmann"
```
I dette eksempelet brukte vi `substring()` metoden til å trekke ut delen av tekststrengen fra og med indeks 5 til og med indeks 12. Det vil si at de tegnene vi får tilbake er "Nordmann", siden det er navnet etter mellomrommet.

En annen måte å gjøre dette på er å bruke `substr()` metoden, som tar inn startindeks og antall tegn som parametere:

```Javascript
let fornavn = navn.substr(0,4); // "Kari"
```

Denne metoden vil hente ut de 4 første tegnene i tekststrengen, som i dette tilfellet er "Kari". Det er viktig å merke seg at `substr()` tar inn lengden på substringen som andre parameter, mens `substring()` tar inn sluttposisjonen.

## Deep Dive

Begge disse metodene fungerer også med negative verdier, som betyr at du kan telle fra slutten av tekststrengen. Hvis du for eksempel vil ha de to siste tegnene i et navn, kan du bruke følgende kode:

```Javascript
let etternavn = navn.substring(-2); // "nn"
```

En annen viktig ting å huske på er at begge disse metodene returnerer en ny tekststreng, og endrer ikke den opprinnelige tekststrengen. Dette betyr at du kan lagre den nye substringen i en variabel og bruke den videre i koden din.

## Se også

- [String.substring()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/substring)
- [String.substr()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/substr)