---
title:                "Beräkning av ett datum i framtiden eller förflutna"
html_title:           "Javascript: Beräkning av ett datum i framtiden eller förflutna"
simple_title:         "Beräkning av ett datum i framtiden eller förflutna"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/javascript/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Beräkning av datum i framtiden eller förfluten tid är en viktig aspekt av programmering eftersom det tillåter utvecklare att skapa dynamiska och användbara funktioner i sina program. Genom att kunna beräkna datum kan man till exempel skapa betalningssystem som automatiskt genererar fakturor baserat på ett specifikt datum eller händelsetracker som håller reda på när ett visst evenemang kommer att äga rum.

## Hur man gör:
Här är kod exempel på hur man kan beräkna ett datum i framtiden eller förfluten tid med hjälp av Javascript.

```Javascript
//Beräkna ett datum i framtiden
const today = new Date();
const futureDate = new Date(today);
futureDate.setDate(today.getDate() + 7); //lägger till 7 dagar till dagens datum
console.log(futureDate); //output: 2021-05-21T09:30:48.828Z

//Beräkna ett datum i förfluten tid
const today = new Date();
const pastDate = new Date(today);
pastDate.setDate(today.getDate() - 7); //subtraherar 7 dagar från dagens datum
console.log(pastDate); //output: 2021-05-07T09:30:48.828Z
```

## Djupdykning:
Beräkning av datum har varit en viktig del av datorprogrammering sedan de första datorerna skapades. Det var en av de första funktionerna som implementerades i programmeringsspråket COBOL, som utvecklades på 1960-talet. Idag finns det flera andra sätt att beräkna datum i framtiden eller förfluten tid, såsom att använda tidsstämplar eller bibliotek som Moment.js.

## Se även:
- [Moment.js](https://momentjs.com/) - ett populärt Javascript-bibliotek för att hantera datum och tid.
- [Date and Time in COBOL](https://www.ibm.com/support/knowledgecenter/en/SS6SGM_5.3.0/com.ibm.entcobol.doc_5.3/tpc/BESRTIME.html) - mer information om beräkning av datum i COBOL.