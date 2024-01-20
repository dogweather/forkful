---
title:                "Analizando una fecha a partir de una cadena de texto"
html_title:           "Bash: Analizando una fecha a partir de una cadena de texto"
simple_title:         "Analizando una fecha a partir de una cadena de texto"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/javascript/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

# Parseo de Fechas en Javascript

## ¿Qué y Por Qué?
Parsear una fecha desde un string permite convertir un texto en un objeto `Date` de Javascript. Los programadores pueden así trabajar con fechas y horas, realizar cálculos y formateos.

## Cómo hacerlo:

Para convertir un string a una fecha, usa el constructor `Date()`. Aquí tienes un ejemplo sencillo.

```Javascript
let fechaString = "2021-12-25";
let fecha = new Date(fechaString);
console.log(fecha);
```

Esto imprimirá la fecha que hemos parseado, algo como `Sat Dec 25 2021 00:00:00 GMT+0200 (hora estándar de España)`.

Si necesitas parsear una fecha y hora específicas, puedes hacer lo siguiente:

```Javascript
let fechaYHoraString = "2021-12-25T10:30:00";
let fechaYHora = new Date(fechaYHoraString);
console.log(fechaYHora);
```
Esto te dará `Sat Dec 25 2021 10:30:00 GMT+0200 (hora estándar de España)`.

## Deep Dive

El objeto `Date` de Javascript se introdujo en ECMAScript 1 y desde entonces ha estado disponible para parseo de fechas y horas. Aunque hay bibliotecas alternativas como Moment.js que ofrecen más funciones, estos métodos nativos son suficientes para la mayoría de los casos de uso.

El constructor `Date()` acepta varios formatos de string, como ISO 8601 ("YYYY-MM-DD") y UTC ("MM, DD, YYYY"). Además, Date parsea la fecha y hora según la zona horaria del usuario. Por lo tanto, dos usuarios en diferentes zonas horarias pueden ver resultados diferentes al parsear la misma cadena.

## Ver También

- Documentación oficial del objeto Date en MDN: [Date - JavaScript | MDN](https://developer.mozilla.org/es/docs/Web/JavaScript/Reference/Global_Objects/Date)
- Un detallado artículo sobre la manipulación de fechas en Javascript: [A Comprehensive Guide to JavaScript Dates](https://flaviocopes.com/javascript-dates/)