---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:23:11.318015-07:00
description: "JSON (JavaScript Object Notation) to lekki format wymiany danych, \u0142\
  atwy do odczytania i zapisania dla ludzi oraz do analizy i generowania dla maszyn.\u2026"
lastmod: '2024-03-13T22:44:35.817689-06:00'
model: gpt-4-0125-preview
summary: "JSON (JavaScript Object Notation) to lekki format wymiany danych, \u0142\
  atwy do odczytania i zapisania dla ludzi oraz do analizy i generowania dla maszyn.\u2026"
title: Praca z JSON
---

{{< edit_this_page >}}

## Co i dlaczego?

JSON (JavaScript Object Notation) to lekki format wymiany danych, łatwy do odczytania i zapisania dla ludzi oraz do analizy i generowania dla maszyn. Programiści używają go do przechowywania i przesyłania danych w aplikacjach internetowych, co czyni go kręgosłupem komunikacji współczesnych API i usług sieciowych.

## Jak to zrobić:

### Parsowanie JSON
Aby przekonwertować ciąg JSON na obiekt JavaScript, użyj `JSON.parse()`.

```javascript
const jsonString = '{"name":"John", "age":30, "city":"New York"}';
const obj = JSON.parse(jsonString);
console.log(obj.name); // Wynik: John
```

### Zamiana obiektów JavaScript na ciągi JSON
Aby przekonwertować obiekt JavaScript z powrotem na ciąg JSON, użyj `JSON.stringify()`.

```javascript
const user = { name: "Jane", age: 25, city: "London" };
const jsonString = JSON.stringify(user);
console.log(jsonString); // Wynik: {"name":"Jane","age":25,"city":"London"}
```

### Praca z plikami w Node.js
Aby odczytać plik JSON i przekonwertować go na obiekt w środowisku Node.js, możesz użyć modułu `fs`. Przykład zakłada, że masz plik o nazwie `data.json`.

```javascript
const fs = require('fs');

fs.readFile('data.json', 'utf-8', (err, data) => {
    if (err) rzuć wyjątek err;
    const obj = JSON.parse(data);
    console.log(obj);
});
```

Aby zapisać obiekt do pliku JSON:

```javascript
const fs = require('fs');
const user = { name: "Mike", age: 22, city: "Berlin" };

fs.writeFile('user.json', JSON.stringify(user, null, 2), (err) => {
    if (err) rzuć wyjątek err;
    console.log('Dane zapisane do pliku');
});
```

### Biblioteki stron trzecich
Do skomplikowanych operacji na JSON, frameworki i biblioteki takie jak `lodash` mogą upraszczać zadania, ale do podstawowych operacji często wystarczają rodzime funkcje JavaScript. Dla dużych lub aplikacji krytycznych pod względem wydajności możesz rozważyć użycie bibliotek takich jak `fast-json-stringify` dla szybszej konwersji na ciągi JSON lub `json5` dla parsowania i konwertowania z użyciem bardziej elastycznego formatu JSON.

Parsowanie za pomocą `json5`:
```javascript
const JSON5 = require('json5');

const jsonString = '{name:"John", age:30, city:"New York"}';
const obj = JSON5.parse(jsonString);
console.log(obj.name); // Wynik: John
```

Te przykłady pokrywają podstawowe operacje z JSON w JavaScript, idealne dla początkujących przechodzących z innych języków i chcących efektywnie obsługiwać dane w aplikacjach internetowych.
