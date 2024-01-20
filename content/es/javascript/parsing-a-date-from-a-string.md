---
title:                "Análisis de una fecha a partir de una cadena"
date:                  2024-01-20T15:37:25.647685-07:00
html_title:           "Arduino: Análisis de una fecha a partir de una cadena"
simple_title:         "Análisis de una fecha a partir de una cadena"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/javascript/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Qué y Por Qué?
Parsear una fecha desde un string significa convertir texto que representa una fecha a un objeto de fecha de JavaScript para que podamos manipular y trabajar con él. Lo hacemos porque las fechas suelen venir como strings de bases de datos o APIs y necesitamos convertirlas para realizar operaciones como comparaciones de fechas o cálculos de tiempo.

## Cómo hacerlo:
Vamos directo al grano con un par de ejemplos simples.

```javascript
const fechaString = '2023-04-12T07:20:50.52Z'; // ISO 8601 format
const fechaObjeto = new Date(fechaString); 
console.log(fechaObjeto); // Muestra la fecha en formato objeto de JavaScript
```
Salida: Wed Apr 12 2023 09:20:50 GMT+0200 (Central European Summer Time)

Si solo tienes la fecha sin la hora, también funciona:

```javascript
const fechaSolo = '2023-04-12'; // solo fecha
const fechaObjetoSimple = new Date(fechaSolo);
console.log(fechaObjetoSimple);
```
Salida: Wed Apr 12 2023 02:00:00 GMT+0200 (Central European Summer Time)

## Profundizando
El parseo de fechas ha sido históricamente un dolor de cabeza debido a la falta de estandarización en los formatos de fecha hasta que ISO 8601 llegó al rescate, ofreciendo un formato coherente para la representación de fechas. Sin embargo, las fechas en JavaScript pueden ser caprichosas debido a la variabilidad de los husos horarios.

Hay alternativas al constructor `Date`, como las bibliotecas `Moment.js` o `date-fns`, que ofrecen más control y funciones para trabajar con fechas. Pero con el update de ECMAScript 2020, JavaScript introdujo `Temporal`, un moderno API para fechas, aún en fase experimental en el momento en que se escribe este artículo.

Una consideración importante al parsear fechas es la zona horaria. Al utilizar `new Date()` con una fecha en formato ISO 8601, JavaScript la trata como UTC. Esto quiere decir que puede haber diferencias si no se maneja adecuadamente la zona horaria cuando se muestra la fecha al usuario.

## Ver También
Para no alargarnos, aquí tienes enlaces útiles que expanden lo que hemos visto:

- MDN Web Docs sobre Date: https://developer.mozilla.org/es/docs/Web/JavaScript/Reference/Global_Objects/Date
- Documentación sobre `Moment.js`: https://momentjs.com/docs/
- `date-fns`, una alternativa ligera a `Moment.js`: https://date-fns.org/
- Propuesta de Temporal API: https://proposal-temporal.now.sh/