---
title:                "Obteniendo la fecha actual"
html_title:           "TypeScript: Obteniendo la fecha actual"
simple_title:         "Obteniendo la fecha actual"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/typescript/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Por qué
Ahora más que nunca, es importante mantener nuestra tecnología actualizada y eficiente. Al obtener la fecha actual en TypeScript, podemos asegurarnos de que nuestras aplicaciones y programas estén sincronizados y funcionando correctamente.

## Cómo hacerlo
Obtener la fecha actual en TypeScript es muy fácil y rápido. Solo sigue estos pasos:

### 1. Importar la función
Primero, necesitamos importar la función Date del módulo de Javascript:

```TypeScript
import {Date} from 'js- fecha';
```

### 2. Crear una instancia de Date
Luego, creamos una instancia de Date que nos dará la fecha y hora actual:

```TypeScript
let fechaActual = new Date();
```

### 3. Obtener la fecha actual
Por último, podemos utilizar los métodos getFullYear(), getMonth() y getDate() para obtener la fecha actual en el formato que deseemos:

```TypeScript
console.log(fechaActual.getFullYear()); // resultado: 2021
```

```TypeScript
console.log(fechaActual.getMonth()); // resultado: 8 (septiembre, ya que los meses comienzan en 0)
```

```TypeScript
console.log(fechaActual.getDate()); // resultado: 17 (día actual)
```

## Deep Dive
La función Date de JavaScript es en realidad un objeto, por lo que también podemos utilizar otros métodos para obtener información detallada sobre la fecha y hora actual. Algunos de estos métodos son:

- getHours(): nos da la hora actual en formato de 24 horas (0-23).
- getMinutes(): nos da los minutos actuales.
- getSeconds(): nos da los segundos actuales.

Además, también podemos modificar la fecha y hora actual utilizando otros métodos como setDate() o setFullYear().

## Ver también
- [Documentación de Date de TypeScript](https://www.typescriptlang.org/docs/handbook/release-notes/typescript-3-0.html#support-for-ecmascript-2018)
- [Introducción a TypeScript](https://www.tutorialspoint.com/typescript/index.htm)
- [Curso de TypeScript para principiantes](https://www.udemy.com/course/typescript-de-cero-a-experto/?referralCode=25A9ECF37F6C3D1F2A55)