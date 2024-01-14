---
title:    "TypeScript: Calculando una fecha en el futuro o pasado"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

## Por qué

Calcular la fecha en el futuro o en el pasado puede ser una tarea necesaria en muchos proyectos de programación. Ya sea para programar eventos, hacer seguimiento del tiempo o realizar cálculos cronológicos, tener la habilidad de calcular fechas en el futuro o en el pasado puede ser muy útil.

## Cómo hacerlo

Para calcular una fecha en el futuro o en el pasado en TypeScript, primero debemos importar la librería 'date-fns'. Asumamos que queremos calcular la fecha en 2 días a partir de hoy:

```TypeScript
import { addDays } from 'date-fns';

const today = new Date();
const futureDate = addDays(today, 2);

console.log(futureDate);
```

Este código creará una nueva variable con la fecha de hoy y luego usará la función `addDays` para añadir 2 días a ella. La función `addDays` es parte de la librería 'date-fns' y recibe como parámetros la fecha original y el número de días que queremos añadir. En este caso, el resultado será la fecha en 2 días a partir de hoy.

Otra forma de calcular fechas en el futuro o en el pasado es usando la función `subDays`, que funciona de manera similar pero permite restar días en lugar de sumarlos. Por ejemplo:

```TypeScript
import { subDays } from 'date-fns';

const today = new Date();
const pastDate = subDays(today, 5);

console.log(pastDate);
```

Este código nos devolverá la fecha de hace 5 días a partir de hoy.

## Profundizando en el tema

La librería 'date-fns' también ofrece muchas otras funciones útiles para manipular fechas. Por ejemplo, podemos sumar o restar años, meses, horas, minutos y segundos usando las funciones `addYears`, `addMonths`, `addHours`, etc.

También podemos utilizar la función `format` para dar formato a nuestras fechas en diferentes formatos como 'dd/MM/yyyy' o 'MM/dd/yyyy'. Por ejemplo:

```TypeScript
import { format } from 'date-fns';

const today = new Date();
const formattedDate = format(today, 'dd/MM/yyyy');

console.log(formattedDate);
```

Este código nos mostraría la fecha actual en el formato 'dd/MM/yyyy'.

Además, la librería 'date-fns' también ofrece funciones para comparar fechas, verificar si una fecha es válida y mucho más. Para obtener una lista completa de funciones y su documentación, se puede acceder a la [página oficial de 'date-fns'](https://date-fns.org/) o revisar la [documentación de TypeScript](https://www.typescriptlang.org/docs/handbook/basic-types.html#date).

## Ver también

- [Página oficial de 'date-fns'](https://date-fns.org/)
- [Documentación de TypeScript sobre fechas](https://www.typescriptlang.org/docs/handbook/basic-types.html#date)