---
title:                "Sammenligner to datoer"
html_title:           "Clojure: Sammenligner to datoer"
simple_title:         "Sammenligner to datoer"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/javascript/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Sammenligne to datoer betyr å bestemme hvilken dato som kommer før eller etter den andre. Programmerere gjør dette for å sortere hendelser, administrere tidsavhengige data eller beregne tidsintervaller.

## Hvordan gjøre det:

Sjekk dette out! Komparere to datoer er så enkelt som pai i Javascript. 

```Javascript
let date1 = new Date(2020, 11, 25);
let date2 = new Date(2022, 11, 25);

if (date1 > date2) {
   console.log("date1 er senere enn date2");
} else if (date1 < date2) {
   console.log("date1 er tidligere enn date2");
} else {
   console.log("date1 og date2 er de samme");
}
```
Output:

```shell
"date1 er tidligere enn date2"
```

## Dypdykk:

Historisk sett har sammenligning av datoer vært en del av programmeringsspråk siden dagene som COBOL ble introdusert. I JavaScript, skjønt, det har vært mulig siden ECMAScript 1 hele veien tilbake i 1997. 

Som for alternativer, kan du også bruke `valueOf()` eller `getTime()` funksjoner for å konvertere datoene til millisekunder siden 1. januar 1970 og deretter sammenligne tallene. 

```Javascript
if(date1.getTime() < date2.getTime()){
   console.log("date1 er tidligere enn date2");
}
```

Når det gjelder implementeringsdetaljer, når du bruker `new Date()`, bruker JavaScript dagens dato og tid som standard. 

## Se også:

1. [MDN Web Docs: Date](https://developer.mozilla.org/nb-NO/docs/Web/JavaScript/Reference/Global_Objects/Date)
2. [ECMAScript 2016: Date Objects](https://www.ecma-international.org/ecma-262/7.0/index.html#sec-date-objects)
3. [StackOverflow: Hvordan sammenligne datoer i JavaScript](https://stackoverflow.com/questions/492994/compare-two-dates-with-javascript)