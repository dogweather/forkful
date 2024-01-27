---
title:                "Arbeid med JSON"
date:                  2024-01-19
html_title:           "Arduino: Arbeid med JSON"
simple_title:         "Arbeid med JSON"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/javascript/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why?
JSON (JavaScript Object Notation) gjør datautveksling enkel. Programmerere bruker det fordi det er lettvekt, tekstbasert og lett å lese både for mennesker og maskiner.

## How to:
```javascript
// Parsing JSON
const jsonString = '{"name":"Ola", "age":30, "city":"Oslo"}';
const user = JSON.parse(jsonString);
console.log(user.name); // Output: Ola

// Stringify JavaScript object to JSON
const userObject = { name: "Kari", age: 29, city: "Bergen" };
const userToJson = JSON.stringify(userObject);
console.log(userToJson); // Output: '{"name":"Kari","age":29,"city":"Bergen"}'
```

## Deep Dive
JSON ble introdusert i 2001, basert på JavaScript, men er nå uavhengig og støttes av mange programmeringsspråk. Alternativer som XML er mer verbose og komplekse. Når man jobber med JSON, handler det ofte om `JSON.parse()` for å lese data og `JSON.stringify()` for å skrive data.

## See Also
- MDN Web Docs JSON guide: [https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/JSON](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/JSON)
- JSON offisiell nettside: [https://www.json.org/json-en.html](https://www.json.org/json-en.html)
