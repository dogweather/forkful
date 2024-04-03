---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:10:12.313184-07:00
description: "JSON, of JavaScript Object Notation, is een lichtgewicht gegevensformaat\
  \ voor het opslaan en transporteren van gegevens. Programmeurs gebruiken het omdat\u2026"
lastmod: '2024-03-13T22:44:51.224144-06:00'
model: gpt-4-0125-preview
summary: JSON, of JavaScript Object Notation, is een lichtgewicht gegevensformaat
  voor het opslaan en transporteren van gegevens.
title: Werken met JSON
weight: 38
---

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
