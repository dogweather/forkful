---
title:    "Javascript: Slette tegn som matcher et mønster"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/javascript/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# Hvorfor: Sletting av tegn som matcher et mønster i Javascript

Å slette tegn som matcher et visst mønster kan være en nyttig funksjon i Javascript-programmering. Dette kan hjelpe i å rydde opp i tekststrenger eller filtrere ut uønskede tegn. For eksempel kan det være nyttig å slette alle tall fra en tekststreng for å bare beholde bokstaver. Å vite hvordan man kan utføre denne handlingen kan også bidra til å lære mer om tekstbehandling i Javascript.

# Hvordan: Eksempler på kode og forventet resultat

For å slette tegn som matcher et mønster, kan man bruke den innebygde metoden "replace()" i Javascript. Denne metoden tar inn to parametre, det første er mønsteret man ønsker å matche, og det andre er det nye tegnet man ønsker å erstatte den matchende teksten med. Denne metoden returnerer en ny streng med de ønskede endringene.

```Javascript
// Eksempel:
let tekst = "Jeg liker å spise frukt og grønnsaker";
let nyTekst = tekst.replace(/[a-z]/gi, ""); 
// Her vil nyTekst være "J S"

// Eksempel med tall:
let tall = "123456789";
let nyTall = tall.replace(/[0-9]/g, ""); 
// Her vil nyTall være en tom streng
```

I dette eksempelet bruker vi "replace()" metoden og et regelmessig uttrykk (regular expression) for å matche alle små bokstaver (a-z) uavhengig av om de er store eller små (gi flagget). Det betyr at alle små og store bokstaver i teksten vil bli slettet og bare hovedbokstavene blir igjen.

# Dypdykk: Mer informasjon om sletting av characters som matcher et mønster

Å bruke regelmessige uttrykk (regular expressions) til å matche et mønster er en kraftig funksjon i Javascript. Dette åpner for forskjellige muligheter til å utføre avanserte manipulasjoner av tekststrenger. I det første eksempelet brukte vi "g" flagget i det regelmessige uttrykket, som står for global. Dette gjør at metoden vil søke etter alle forekomster av mønsteret i hele teksten. Hvis vi bare ønsker å slette det første treffet, kan vi bruke "i" flagget for å gjøre det case-insensitive, eller "m" flagget for å søke i flere linjer.

Et annet viktig konsept å forstå er escape-tegn (backslash) når man jobber med regelmessige uttrykk. Hvis tekststrengen inneholder spesielle tegn som også brukes i regelmessige uttrykk, må disse escapes for å unngå å endre funksjonaliteten. For eksempel, hvis vi vil matche parenteser ( ) i teksten, må vi bruke backslash foran dem: /\\(\\)/.

# Se også

- [MDN Web Docs: replace()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/replace)
- [W3Schools: Regular Expressions](https://www.w3schools.com/jsref/jsref_obj_regexp.asp)
- [Eloquent JavaScript: Regular Expressions](https://eloquentjavascript.net/09_regexp.html)