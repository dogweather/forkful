---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:23:18.444890-07:00
description: "C\xF3mo hacerlo: Para convertir una cadena JSON en un objeto de JavaScript,\
  \ utiliza `JSON.parse()`."
lastmod: '2024-03-13T22:44:59.479778-06:00'
model: gpt-4-0125-preview
summary: Para convertir una cadena JSON en un objeto de JavaScript, utiliza `JSON.parse()`.
title: Trabajando con JSON
weight: 38
---

## Cómo hacerlo:


### Analizando JSON
Para convertir una cadena JSON en un objeto de JavaScript, utiliza `JSON.parse()`.

```javascript
const jsonString = '{"name":"John", "age":30, "city":"New York"}';
const obj = JSON.parse(jsonString);
console.log(obj.name); // Salida: John
```

### Convirtiendo objetos de JavaScript en cadenas JSON
Para convertir un objeto de JavaScript de nuevo en una cadena JSON, utiliza `JSON.stringify()`.

```javascript
const user = { name: "Jane", age: 25, city: "London" };
const jsonString = JSON.stringify(user);
console.log(jsonString); // Salida: {"name":"Jane","age":25,"city":"London"}
```

### Trabajando con archivos en Node.js
Para leer un archivo JSON y convertirlo en un objeto en un entorno Node.js, puedes usar el módulo `fs`. Este ejemplo supone que tienes un archivo llamado `data.json`.

```javascript
const fs = require('fs');

fs.readFile('data.json', 'utf-8', (err, data) => {
    if (err) throw err;
    const obj = JSON.parse(data);
    console.log(obj);
});
```

Para escribir un objeto en un archivo JSON:

```javascript
const fs = require('fs');
const user = { name: "Mike", age: 22, city: "Berlin" };

fs.writeFile('user.json', JSON.stringify(user, null, 2), (err) => {
    if (err) throw err;
    console.log('Datos escritos en el archivo');
});
```

### Bibliotecas de terceros
Para operaciones complejas con JSON, marcos y bibliotecas como `lodash` pueden simplificar las tareas, pero para operaciones básicas, a menudo son suficientes las funciones nativas de JavaScript. Para aplicaciones a gran escala o críticas en términos de rendimiento, puedes considerar bibliotecas como `fast-json-stringify` para una serialización JSON más rápida o `json5` para análisis y serialización utilizando un formato JSON más flexible.

Analizando con `json5`:
```javascript
const JSON5 = require('json5');

const jsonString = '{name:"John", age:30, city:"New York"}';
const obj = JSON5.parse(jsonString);
console.log(obj.name); // Salida: John
```

Estos ejemplos cubren operaciones básicas con JSON en JavaScript, perfectos para principiantes que hacen la transición desde otros lenguajes y buscan manejar datos en aplicaciones web de manera eficiente.
