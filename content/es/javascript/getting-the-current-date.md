---
title:                "Obteniendo la fecha actual"
html_title:           "C#: Obteniendo la fecha actual"
simple_title:         "Obteniendo la fecha actual"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/javascript/getting-the-current-date.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué? 

Obtener la fecha actual en Javascript es un procedimiento mediante el cual se recoge el día, mes y año actual. Los programadores lo utilizan para rastrear eventos, establecer plazos y proporcionar marcas de tiempo en las aplicaciones web.

## ¿Cómo hacerlo?

Para obtener la fecha y la hora actuales en Javascript, simplemente necesitas crear una nueva instancia de la clase Date.

```Javascript
let fecha = new Date();
console.log(fecha);
```

En la consola, deberías ver algo como esto: 

```Javascript
Tue Aug 17 2021 11:09:34 GMT+0300 (hora de la Europa oriental)
```

Puedes obtener partes específicas de la fecha como el día, mes y año usando los métodos de la clase Date.

```Javascript
let fecha = new Date();
let dia = fecha.getDate();
let mes = fecha.getMonth() + 1; // Se suma 1 porque los meses en Javascript empiezan desde 0
let año = fecha.getFullYear();

console.log(dia, mes, año); 
```

Esto imprimirá el día, el mes y el año actual, separados por espacios.

## Análisis en profundidad

El objeto de fecha y hora en Javascript se introdujo en ECMAScript 1, la primera edición de Javascript, publicada en junio de 1997. 

Hay varias bibliotecas alternativas para trabajar con fechas y horas en Javascript. Moment.js ha sido históricamente la más popular, pero ha sido oficialmente desaconsejada. Otras bibliotecas como Day.js y date-fns están ganando popularidad y se recomiendan para nuevos proyectos.

Al usar la clase Date, debes tener en cuenta que los meses en Javascript comienzan desde 0 (enero es 0, febrero es 1, etc.), por lo que se debe recordar sumar 1 cuando se muestra el mes al usuario.

## Ver también

Para una mayor comprensión y mejores prácticas al trabajar con fechas y horas en Javascript, puedes consultar los siguientes enlaces:

- [MDN Web Docs - Date](https://developer.mozilla.org/es/docs/Web/JavaScript/Referencia/Objetos_globales/Date)
- [You Don't Need Moment.js](https://youmightnotneed.com/momentjs/)
- [date-fns - Modern JavaScript date utility library](https://date-fns.org/)
- [day.js - 2kB JavaScript date utility library](https://day.js.org/)