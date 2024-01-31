---
title:                "Obteniendo la fecha actual"
date:                  2024-01-20T15:15:04.878084-07:00
html_title:           "Bash: Obteniendo la fecha actual"
simple_title:         "Obteniendo la fecha actual"

category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/javascript/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Qué y Por Qué?
Obtener la fecha actual en Javascript es acceder al momento presente en el código. Los programadores lo hacen para registros, funciones de tiempo real y todo lo que necesita una marcación temporal.

## Cómo hacerlo:
Aquí tienes el cómo. Simple y directo:

```Javascript
// Obtener la fecha y hora actuales
const ahora = new Date();

// Mostrar la fecha y hora
console.log(ahora);
```

Si corres eso, obtienes algo como:

```
2023-04-01T12:34:56.789Z
```

Eso es todo. Ahora sabes la hora exacta.

## Profundizando
Breve historia: JavaScript nació en 1995. Desde entonces, `Date` ha estado allí, aunque ha evolucionado.

Alternativas: Además de `new Date()`, librerías como Moment.js o date-fns ofrecen más funciones y flexibilidad, pero muchas veces son innecesarias para tareas sencillas.

Detalles de implementación: `Date` en JavaScript usa el tiempo del sistema. Zonas horarias pueden complicar las cosas. La fecha/hora es UTC, universal.

## Ver También
- MDN Web Docs sobre `Date`: https://developer.mozilla.org/es/docs/Web/JavaScript/Reference/Global_Objects/Date
- Documentación de Moment.js: https://momentjs.com/docs/
- Documentación de date-fns: https://date-fns.org/docs/Getting-Started
