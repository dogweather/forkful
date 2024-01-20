---
title:                "Utdrag av understrenger"
html_title:           "Bash: Utdrag av understrenger"
simple_title:         "Utdrag av understrenger"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/javascript/extracting-substrings.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å utvinne delstrenger (substrings) i JavaScript er å hente ut en del av en tekststreng eller tegnrekke. Det gjøres for å manipulere, sammenligne eller analysere data mer effektivt.

## Hvordan til:

Her er noen kodeeksempler på hvordan du kan utvinne delstrenger i JavaScript:

```Javascript
let str = "Hei, verden!";
let subStr1 = str.substring(0, 3);
let subStr2 = str.slice(0, -1);
console.log(subStr1); // Viser: "Hei"
console.log(subStr2); // Viser: "Hei, verden"
```

Note: `substring()` og `slice()` kan virke like, men de håndterer negative indekser og utvekslte argumenter annerledes.

## Dypdykk

Historisk sett, har utvinning av delstrenger eksistert siden de tidlige dager av programmering for å effektivisere tekstmanipulering og dataanalyse. JavaScript, som kom i 1995, gjorde det enda enklere.

Alternativene til de innebygde metodene `.substring()` og `.slice()` inkluderer `.substr()`, men det er nå utdatert. Du kan også bruke regulære uttrykk for mer komplekse utdrag.

Implementeringsdetaljer knyttet til utvinning av delstrenger involverer forståelse av hvordan indekser fungerer i JavaScript. Indekser starter fra posisjon 0 og går oppover. Negative indekser brukt i `.slice()` refererer til posisjonene fra slutten av strengen.

## Se Også:

For mer informasjon om hvordan du manipulerer tegnstrenger i JavaScript, sjekk ut følgende ressurser:

- [MDN Web Docs: String](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String)
- [W3Schools: JavaScript String Methods](https://www.w3schools.com/js/js_string_methods.asp)
- [JavaScript.info: Strings](https://javascript.info/string)