---
title:                "Beräkna ett datum i framtiden eller förflutna"
html_title:           "Javascript: Beräkna ett datum i framtiden eller förflutna"
simple_title:         "Beräkna ett datum i framtiden eller förflutna"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/javascript/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Vad och varför?

Beräkning av ett datum i framtiden eller förflutna är processen att lägga till eller dra ifrån dagar, månader eller år från ett existerande datum. Programmerare gör detta för att hantera tidrelaterade funktioner, som event planering eller betalningssystem.

## Hur man gör:

Här kommer vi att använda två metoder: `setDate()` och `getDate()`. Se koden nedan:

```Javascript
// Skapa ett nytt datumobjekt
let nu = new Date(); 

// Få dagens dag
let dag = nu.getDate();

// Beräkna ett datum 5 dagar framåt
nu.setDate(dag + 5);

console.log(nu);
```

Om du kör den här koden kommer du att se en utmatning som liknar detta:

```Javascript
2022-09-02T17:41:08.334Z
```

Om man vill räknar tillbaka de dagar, ändra `+` tecknet till `-`.

## Fördjupning

Beräknade datum har varit en del av Javascript sedan tidiga versioner av språket. Andra metoder för beräkning av datum inkluderar att använda moment.js biblioteket eller date-fns biblioteket, vilka ger smidigare funktionaliteter för datum och tidhantering.

Om du behöver göra mer komplexa datum operationer kan du använda Date-fns biblioteket. Det erbjuder funktioner som addDays, subtractDays, addMonths och subtractMonths, vilka gör hantering av datum mycket mer lättare. 

## Se även

För mer information och andra relaterade ämnen, kolla på följande länkar:

1. MDN web docs: [Date Objects](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)

2. JavaScript Date Reference: [JavaScript Date Object](https://www.w3schools.com/jsref/jsref_obj_date.asp)

3. Moment.js: [Manipulating dates](https://momentjs.com/docs/#/manipulating/)

4. Date-fns: [Manage Dates](https://date-fns.org/v2.21.3/docs/)