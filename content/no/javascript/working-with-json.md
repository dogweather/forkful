---
title:                "Jobbe med json"
html_title:           "Javascript: Jobbe med json"
simple_title:         "Jobbe med json"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/javascript/working-with-json.md"
---

{{< edit_this_page >}}

# Hva & Hvorfor?
Working with JSON er en måte for programmører å lese og manipulere data i et enkelt og strukturert format. JSON, som står for JavaScript Object Notation, brukes ofte til å lagre og overføre data mellom en klient og en server. Det er et populært valg blant programmører på grunn av sin enkelhet og lesbarhet.

# Hvordan:
For å lese og manipulere data i JSON-format, kan du bruke "JSON" objektet i JavaScript. Dette objektet har to nyttige metoder: "parse" og "stringify". La oss se på et eksempel på hver:

```javascript
// parse funksjonen konverterer JSON-stringen til et JavaScript objekt
let data = JSON.parse('{"navn": "Sara", "alder": 25}');

// stringify funksjonen konverterer JavaScript objektet til en JSON-string
let jsonData = JSON.stringify({navn: 'John', alder: 30});

// output: Sara er 25 år gammel.
console.log(`${data.navn} er ${data.alder} år gammel.`);

// output: {"navn": "John", "alder": 30}
console.log(jsonData);
```

# Dypdykk:
JSON ble utviklet av Douglas Crockford i 2001 og har raskt blitt et populært format for datautveksling på grunn av sin enkelhet og lesbarhet. Det finnes også alternative formater som XML og YAML, men JSON er i stor grad foretrukket av utviklere for sin enkelhet og godt grep om datastrukturen.

Når det gjelder implementering, er det viktig å huske at JSON er et strengformat, og at det er strengt syntaksisk gjeldende. Dette betyr at du må være nøye med å følge riktig format når du skriver og leser JSON-data. Det finnes også flere biblioteker og verktøy som gjør det enklere å arbeide med JSON i forskjellige programmeringsspråk.

# Se også:
- [W3Schools JSON tutorial](https://www.w3schools.com/js/js_json_intro.asp)
- [Mozilla Developer Network JSON documentation](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/JSON)