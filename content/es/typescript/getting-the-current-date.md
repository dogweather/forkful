---
title:                "TypeScript: Obteniendo la fecha actual."
simple_title:         "Obteniendo la fecha actual."
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/typescript/getting-the-current-date.md"
---

{{< edit_this_page >}}

## ¿Por qué obtener la fecha actual en TypeScript?

Obtener la fecha actual es una funcionalidad básica y esencial en cualquier lenguaje de programación. En TypeScript, la obtención de la fecha actual permite a los desarrolladores manipular y mostrar la fecha en el formato deseado, así como realizar cálculos de tiempo. Además, es una habilidad fundamental para construir aplicaciones que dependen del tiempo, como calendarios o recordatorios.

## Cómo obtener la fecha actual en TypeScript

Para obtener la fecha actual en TypeScript, se puede utilizar el objeto `Date` incorporado en JavaScript. Este objeto contiene métodos que nos permiten obtener diferentes aspectos de la fecha, como el año, mes, día, hora, minutos y segundos.

Para obtener la fecha actual, simplemente tenemos que instanciar un objeto `Date` y luego utilizar sus métodos para obtener los elementos específicos de la fecha. Un ejemplo de código sería el siguiente:

```
TypeScript
const currentDate = new Date();

console.log(`La fecha actual es: ${currentDate.getDate()}/${currentDate.getMonth() + 1}/${currentDate.getFullYear()}`);
// La fecha actual es: 27/9/2021
```

En el ejemplo anterior, se utiliza el método `getDate()` para obtener el día del mes, `getMonth()` para obtener el mes (teniendo en cuenta que los meses comienzan en 0 en JavaScript) y `getFullYear()` para obtener el año.

## Inmersión profunda en la obtención de la fecha actual

Cuando se instancia un objeto `Date` en TypeScript, se obtiene la fecha y hora actual según la zona horaria del sistema en el que se está ejecutando el código. Sin embargo, también es posible obtener la fecha y hora en una zona horaria específica.

Para ello, se puede utilizar el método `toLocaleString()` y pasar como argumento la zona horaria deseada. Por ejemplo:

```
TypeScript
const currentDate =  new Date().toLocaleString('es-ES', { timeZone: 'Europe/Madrid' });

console.log(currentDate);
// 27/9/2021, 14:30:15
```

Además, el objeto `Date` también cuenta con métodos para manipular y operar con fechas. Por ejemplo, el método `setHours()` nos permite establecer la hora actual, el método `setFullYear()` nos permite establecer el año y así sucesivamente. Esto es útil si necesitamos realizar cálculos o ajustar la fecha actual en una aplicación.

## Vea también

- [Documentación oficial de TypeScript sobre el objeto Date](https://www.typescriptlang.org/docs/handbook/utility-types.html#includetypes)
- [Ejemplos de uso del objeto Date en TypeScript](https://www.rokab.fr/blog/2020/03/20/typescript-get-the-current-time-date-the-right-way/)
- [Tutorial de TypeScript para principiantes](https://www.freecodecamp.org/news/learn-typescript-in-5-minutes-13eda868daeb/)