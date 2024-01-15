---
title:                "Uttrekking av substringer"
html_title:           "Javascript: Uttrekking av substringer"
simple_title:         "Uttrekking av substringer"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/javascript/extracting-substrings.md"
---

{{< edit_this_page >}}

## Hvorfor?

Har du noen gang lurt på hvordan du kan hente ut en del av en tekststreng? Det er her substring-funksjonen i Javascript kommer til nytte! Enten du trenger å manipulere tekst eller bare vil hente ut en del av den, kan substring-funksjonen være en nyttig verktøy i JavaScript-programmering.

## Hvordan gjøre det?

Det første du må gjøre er å definere teksten du vil ekstrahere en substring fra. Du kan gjøre dette ved å opprette en variabel som inneholder teksten. For eksempel:

```Javascript
let tekst = "Denne teksten vil jeg ekstrahere en del av";
```

Deretter må du velge start- og sluttpunktene til substringen du ønsker å hente ut. Dette kan gjøres ved å bruke substring-funksjonen og angi indeksene til start- og sluttpunktene. Indeksene i JavaScript starter alltid på 0.

```Javascript
let deltekst = tekst.substring(5, 12);
```

Dette vil hente ut tekst fra indeks 5 til 12, som i dette tilfellet vil si "teksten". Du kan også angi bare startindeksen, og da vil substringen hentes ut fra denne indeksen til slutten av teksten. For eksempel:

```Javascript
let deltekst = tekst.substring(20);
```

Dette vil hente ut tekst fra indeks 20 og ut til slutten av teksten, som i dette tilfellet vil si "ekstrahere en del av". Du kan også bruke negative indekser, som teller bakfra i teksten. Ved å bruke en negativ indeks for sluttpunktet, vil substringen hentes ut fra starten av teksten til denne indeksen fra slutten. For eksempel:

```Javascript
let deltekst = tekst.substring(10, -5);
```

Dette vil hente ut tekst fra indeks 10 og bakover, og avslutte 5 tegn før slutten av teksten. I dette tilfellet vil det resultere i "vil jeg ekstra".

## Dypdykk

Det er også noen ting du bør være klar over når du bruker substring-funksjonen. For det første vil den alltid returnere en ny streng med den ekstraherte delen. Den vil ikke endre den opprinnelige strengen, så husk å lagre den nye substringen i en variabel hvis du ønsker å bruke den senere.

Det er også verdt å merke seg at det er forskjellige måter å definere start- og sluttpunktene for substringen på. Du kan for eksempel bruke metoden charAt() for å hente ut en bestemt karakter basert på indeks, og deretter bruke indeksene til å definere start- og sluttpunktene for substringen.

En annen ting å huske på er at substring-funksjonen er case-sensitive. Dette betyr at den tar hensyn til store og små bokstaver når den henter ut substringen. Så hvis du ønsker å hente ut en del av teksten basert på en bestemt tekst, må du sørge for at du skriver den nøyaktig lik som den forekommer i teksten.

## Se også

For mer informasjon om substring-funksjonen og andre nyttige metoder for å håndtere tekst i JavaScript, kan du sjekke ut disse lenkene:

[Mozilla Developer Network - substring()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/substring)

[W3Schools - JavaScript String Substring() Method](https://www.w3schools.com/jsref/jsref_substring.asp)