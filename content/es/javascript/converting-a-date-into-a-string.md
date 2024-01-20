---
title:                "Convirtiendo una fecha en una cadena de texto"
html_title:           "C++: Convirtiendo una fecha en una cadena de texto"
simple_title:         "Convirtiendo una fecha en una cadena de texto"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/javascript/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

# Convertir Fecha en Cadena en Javascript

## ¿Qué y Por Qué?
Convertir una fecha en una cadena en Javascript implica transformar un objeto Fecha en un formato legible como "DD-MM-YYYY" o "Día, Mes, Año". Los programadores lo hacen para mostrar la fecha de forma amigable al usuario y facilitar comparaciones entre fechas.

## ¿Cómo se hace?
Las fechas en Javascript se convierten en cadenas usando los métodos incorporados. Las cadenas resultantes se pueden formatear como desees. 

```Javascript
// Crear una nueva fecha
let fechaActual = new Date();

// Conviértelo a una cadena
let fechaCadena = fechaActual.toString();

console.log(fechaCadena);
```

Esto imprimirá algo como: `Tue Aug 24 2021 18:15:52 GMT+0200 (hora de verano de Europa central)`.

Si necesitas un formato específico, puedes usar métodos como `toLocaleDateString()`, `toISOString()`, entre otros. Por ejemplo:

```Javascript
let fechaFormatoLocal = fechaActual.toLocaleDateString('es-ES');
console.log(fechaFormatoLocal);
```
Esto imprimirá la fecha en formato local español, como: `24/8/2021`

## Deep Dive
El objeto `Date` ha sido parte de Javascript desde sus primeros días. Proporcionó formas rudimentarias de manejar la fecha y la hora. Sin embargo, los métodos modernos de conversión y manejo de fechas se añadieron gradualmente para facilitar las tareas comunes.

Un camino alternativo para la conversión es mediante bibliotecas de terceros. Moment.js tiene una rica funcionalidad para manipular fechas, incluyendo la conversión en cadena. Sin embargo, `Date` en ES6 (ECMAScript 2015) y sus versiones posteriores han reducido la necesidad de bibliotecas externas.

El método `toString()` devuelve una cadena en formato IETF (Internet Engineering Task Force). Esto no es necesariamente amigable para el usuario, pero es útil para el registro y depuración. Los métodos locales y ISO, en cambio, permiten mostrar la fecha de una manera que el público objetivo entiende.

## Ver También
[Información de la Documentación MDN sobre el objeto Date](https://developer.mozilla.org/es/docs/Web/JavaScript/Reference/Global_Objects/Date)

[Documentación de Moment.js](https://momentjs.com/docs/#/displaying/as-string/)

[La Especificación ECMAScript 2015](http://www.ecma-international.org/ecma-262/6.0/)