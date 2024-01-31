---
title:                "Lavorare con JSON"
date:                  2024-01-19
html_title:           "Arduino: Lavorare con JSON"
simple_title:         "Lavorare con JSON"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/javascript/working-with-json.md"
---

{{< edit_this_page >}}

## Cosa e Perché?
JSON (JavaScript Object Notation) è un formato di scambio dati leggero. I programmatori lo adorano per la sua semplicità e l'ampia compatibilità tra diversi sistemi e linguaggi di programmazione.

## Come Fare:
Per lavorare con JSON in JavaScript, usa `JSON.parse()` per convertire una stringa JSON in un oggetto e `JSON.stringify()` per convertire un oggetto in una stringa JSON.

```Javascript
// Conversione da stringa JSON a oggetto JavaScript
let jsonData = '{"name": "Mario", "age": 30}';
let obj = JSON.parse(jsonData);

console.log(obj.name); // Output: Mario

// Conversione da oggetto JavaScript a stringa JSON
let jsonObject = { name: "Luigi", age: 25 };
let jsonString = JSON.stringify(jsonObject);

console.log(jsonString); // Output: {"name":"Luigi","age":25}
```

## Approfondimento:
JSON fu introdotto nel 2001 da Douglas Crockford e rapidamente si è imposto come standard de facto per lo scambio di dati. Prima di JSON, XML era il formato principale ma era più verboso e complicato da analizzare. JSON è basato su un sottoinsieme della notazione degli oggetti di JavaScript, ma è indipendente dal linguaggio e può essere utilizzato anche in altri contesti, come Python, Ruby o Java.

## Vedere Anche:
- Documentazione MDN su JSON: [MDN JSON](https://developer.mozilla.org/it/docs/Web/JavaScript/Reference/Global_Objects/JSON)
- "Introducing JSON" di Douglas Crockford: [json.org](http://json.org/)
- Confronto tra JSON e XML: [JSON vs XML](https://www.w3schools.com/js/js_json_xml.asp)
