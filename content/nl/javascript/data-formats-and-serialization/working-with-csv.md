---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:10:06.780424-07:00
description: "Hoe te: CSV bestaat al sinds de vroege dagen van het computergebruik\
  \ - gemakkelijk voor machines om te verwerken en voor mensen om te begrijpen. Maar\
  \ het\u2026"
lastmod: '2024-04-05T22:51:04.008786-06:00'
model: gpt-4-0125-preview
summary: CSV bestaat al sinds de vroege dagen van het computergebruik - gemakkelijk
  voor machines om te verwerken en voor mensen om te begrijpen.
title: Werken met CSV
weight: 37
---

## Hoe te:
**CSV naar JSON parseren:**
```javascript
const csv = `naam,leeftijd,stad
Alice,30,New York
Bob,22,Los Angeles`;

function csvToJson(csv) {
  const regels = csv.split("\n");
  const koppen = regels[0].split(",");
  return regels.slice(1).map(regel => {
    const gegevens = regel.split(",");
    return koppen.reduce((obj, volgendeSleutel, index) => {
      obj[volgendeSleutel] = gegevens[index];
      return obj;
    }, {});
  });
}

console.log(csvToJson(csv));
// Uitvoer: [{naam: 'Alice', leeftijd: '30', stad: 'New York'}, {naam: 'Bob', leeftijd: '22', stad: 'Los Angeles'}]
```

**CSV van JSON genereren:**
```javascript
const jsonData = [
  { naam: "Alice", leeftijd: 30, stad: "New York" },
  { naam: "Bob", leeftijd: 22, stad: "Los Angeles" }
];

function jsonToCsv(json) {
  const koppen = Object.keys(json[0]).join(",");
  const rijen = json.map(obj =>
    Object.values(obj).join(",")
  ).join("\n");
  return `${koppen}\n${rijen}`;
}

console.log(jsonToCsv(jsonData));
// Uitvoer: naam,leeftijd,stad
//         Alice,30,New York
//         Bob,22,Los Angeles
```

## Diepere duik
CSV bestaat al sinds de vroege dagen van het computergebruik - gemakkelijk voor machines om te verwerken en voor mensen om te begrijpen. Maar het is niet perfect. Als je gegevens complex of genest zijn, kunnen JSON of XML een betere keuze zijn. Wat betreft de implementatie, had het omgaan met CSV in JavaScript zijn werk rondes nodig vanwege het gebrek aan een standaardbibliotheek hiervoor; echter, tegenwoordig vereenvoudigen talrijke bibliotheken zoals PapaParse of csv-parser deze taak. Ook kunnen randgevallen zoals nieuwe regeltekens binnen velden en karaktercodering de omgang met CSV gecompliceerd maken en zorgvuldige codering vereisen.

## Zie ook
- MDN Web Docs over Fetch API: https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API/Using_Fetch (CSV-gegevens van het web halen)
- PapaParse: https://www.papaparse.com/ (Robuuste CSV-parser voor de browser)
- RFC 4180: https://tools.ietf.org/html/rfc4180 (Standaarden voor CSV-bestanden)
