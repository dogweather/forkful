---
title:                "Obteniendo la fecha actual"
date:                  2024-01-20T15:16:50.729847-07:00
simple_title:         "Obteniendo la fecha actual"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/typescript/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Qué y Por Qué?
Obtener la fecha actual en TypeScript significa capturar el momento presente en que se ejecuta el código. Lo hacemos para marcar eventos, comparar fechas o simplemente mostrar cuándo sucede algo.

## Cómo Se Hace:
Para obtener la fecha actual en TypeScript, puedes utilizar el objeto `Date` así:

```typescript
const ahora = new Date();
console.log(ahora);
```

Resultado de ejemplo:
```
2023-04-02T14:20:30.045Z
```

Si solo te interesa la fecha sin la hora:

```typescript
const hoy = new Date();
console.log(hoy.toDateString());
```

Resultado de ejemplo:
```
Sun Apr 02 2023
```

Y si quieres la hora en un formato legible:

```typescript
const horaActual = new Date();
console.log(horaActual.toLocaleTimeString());
```

Resultado de ejemplo:
```
2:20:30 PM
```

## Análisis Detallado:
El objeto `Date` de JavaScript, que también usamos en TypeScript, existe desde que se creó JavaScript en los años 90. Es parte del estándar ECMAScript, y maneja fechas basándose en el tiempo universal coordinado (UTC).

Hay alternativas para lidiar con fechas como bibliotecas `Moment.js` o `date-fns`. Estas ofrecen funciones más potentes y flexibles para formatear fechas y realizar cálculos complejos.

A nivel de implementación, cuando creas una nueva instancia de `Date`, internamente capturas el número de milisegundos desde la medianoche del 1 de enero de 1970 UTC. Esto se conoce como "Unix Epoch". Las manipulaciones de fecha y hora se hacen a partir de esta referencia.

## Ver También:
- Documentación de Mozilla Developer Network sobre `Date`: https://developer.mozilla.org/es/docs/Web/JavaScript/Reference/Global_Objects/Date
- Moment.js para manejo avanzado de fechas: https://momentjs.com/
- `date-fns` para operaciones con fechas: https://date-fns.org/
- UTC y Unix Epoch Time: https://www.w3.org/TR/NOTE-datetime
