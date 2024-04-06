---
date: 2024-01-20 17:51:10.611453-07:00
description: "C\xF3mo Hacerlo: Interpolar en JavaScript? Pan comido con *template\
  \ literals*. Aqu\xED tienes ejemplos."
lastmod: '2024-04-05T22:40:45.939028-06:00'
model: gpt-4-1106-preview
summary: Interpolar en JavaScript?
title: "Interpolaci\xF3n de cadenas de texto"
weight: 8
---

## Cómo Hacerlo:
Interpolar en JavaScript? Pan comido con *template literals*. Aquí tienes ejemplos:

```javascript
let nombre = 'Miguel';
let saludo = `Hola, ${nombre}! ¿Cómo estás?`;
console.log(saludo); // "Hola, Miguel! ¿Cómo estás?"

let precio = 9.99;
let producto = 'libro';
let mensaje = `El precio del ${producto} es ${precio} euros.`;
console.log(mensaje); // "El precio del libro es 9.99 euros."
```

Fácil, directo y legible.

## Profundizando
Antes de ES6, ese mundillo de JavaScript era más enredado. Usábamos la concatenación:

```javascript
let nombre = 'Miguel';
let saludo = 'Hola, ' + nombre + '! ¿Cómo estás?';
```

Bien, pero no tan elegante. Luego llegó ES6 en 2015 y nos trajo los *template literals* (o plantillas literales) con los que simplemente encierras tu cadena en backticks (`` ` ``) y magia, interpolas con `${}`.

Más poder aún: puedes meter cualquier expresión de JavaScript dentro de las llaves. Hagamos matemáticas:

```javascript
let a = 5;
let b = 10;
console.log(`Quince es ${a + b} y no ${2 * a + b}.`);
// "Quince es 15 y no 20."
```

Y no termina ahí; funciones, operaciones complejas, llamadas a métodos... todo eso cabe en los `${}`. Pero, ojo, que todo tiene su costo. Abusar de la interpolación con operaciones complicadas puede volver tu código un enredo.

## Ver También
- MDN Web Docs sobre Template Literals: https://developer.mozilla.org/es/docs/Web/JavaScript/Reference/Template_literals
- Detalles sobre ES6: https://www.ecma-international.org/ecma-262/6.0/
- Una guía sobre "Tagged templates", una función avanzada de los literales templados: https://developer.mozilla.org/es/docs/Web/JavaScript/Reference/Template_literals#Etiquetas_de_plantillas
