---
title:                "Werken met JSON"
date:                  2024-01-28T22:10:12.313184-07:00
model:                 gpt-4-0125-preview
simple_title:         "Werken met JSON"

category:             "Javascript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/javascript/working-with-json.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat en Waarom?
JSON, of JavaScript Object Notation, is een lichtgewicht gegevensformaat voor het opslaan en transporteren van gegevens. Programmeurs gebruiken het omdat het gemakkelijk te lezen/schrijven is voor mensen, en machines kunnen het snel parseren en genereren.

## Hoe te:
JSON parseren in JavaScript:
```javascript
const jsonString = '{"name":"John", "age":30, "city":"New York"}';
const user = JSON.parse(jsonString);
console.log(user.name); // Output: John
```

Een JavaScript-object naar JSON omzetten:
```javascript
const user = { name: 'John', age: 30, city: 'New York' };
const jsonString = JSON.stringify(user);
console.log(jsonString); // Output: '{"name":"John","age":30,"city":"New York"}'
```

## Diepgaande Duik
JSON is afgeleid van JavaScript maar is nu een taalonafhankelijk formaat. Er bestaan veel alternatieven zoals XML, maar de minimale syntaxis van JSON heeft zijn populariteit gewonnen voor API-payloads. Technisch gezien is JSON een subset van de JavaScript object literal notatie met enkele verschillen, zoals dat sleutels in dubbele aanhalingstekens moeten worden gewikkeld.

## Zie Ook
- MDN JSON: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/JSON
- JSON Formatter & Validator: https://jsonlint.com/
- JSON vs. XML: https://www.w3schools.com/js/js_json_xml.asp
