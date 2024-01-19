---
title:                "Jämför två datum"
html_title:           "Arduino: Jämför två datum"
simple_title:         "Jämför två datum"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/javascript/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Jämförande av två datum i programmering innebär att fastställa vilket datum som kommer först. Det är viktigt för att hantera och beräkna datumintervall, uppgiftplanering och tidsspårning.

## Hur gör man:

Du kan jämföra två datum i Javascript med hjälp av nedanstående kod:

``` Javascript
let datum1 = new Date('2019-01-01');
let datum2 = new Date('2019-12-31');

if (datum1 > datum2) {
  console.log('datum1 är senare än datum2');
} else if (datum1 < datum2) {
  console.log('datum2 är senare än datum1');
} else {
  console.log('datum1 och datum2 är samma dag');
}
```

Kodens utdata ser ut som detta:

``` Javascript
'datum2 är senare än datum1'
```

## Fördjupning

Att jämföra data är ett grundläggande koncept inom programmering som har sina rötter i dagar av tidig databehandling. Jämförandet av datum har en nyans eftersom datum har en inneboende hierarki (år > månad > dag).

Alternativt kan du använda getTime-funktionen för att omvandla datum till millisekunder sedan Unix Epoch och jämföra dessa:

``` Javascript
if (datum1.getTime() > datum2.getTime()) {
  ...
}
```

You kan också använda `valueOf()` funktionen.

## Se även

Referera till dessa länkar för att få mer information -
- MDN Web Docs: [https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date]()
- W3Schools: [https://www.w3schools.com/js/js_date_methods.asp]()