---
title:                "Obteniendo la fecha actual."
html_title:           "TypeScript: Obteniendo la fecha actual."
simple_title:         "Obteniendo la fecha actual."
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/typescript/getting-the-current-date.md"
---

{{< edit_this_page >}}

¿Qué y por qué?

Obtener la fecha actual es una tarea común para los programadores. Esto se refiere a obtener la fecha y hora actual en que se ejecuta un programa. Los programadores a menudo necesitan esta información para realizar tareas como registrar eventos y transacciones, o para mostrar la fecha y hora actual en una interfaz de usuario.

¡Cómo hacerlo!

```TypeScript
const currentDate = new Date();
```

Este código crea una nueva instancia de la clase `Date` en TypeScript que representa la fecha y hora actual. A continuación, podemos acceder a diferentes métodos de la clase `Date` para obtener información específica, como el día, mes, año, hora, minuto, segundo, etc.

```TypeScript
console.log(currentDate.toDateString());
```
```
Salida esperada: Fri Dec 10 2021
```

Profundizando

En el pasado, obtener la fecha y hora actual era una tarea mucho más compleja y requería escribir códigos personalizados. Sin embargo, gracias a los avances en el lenguaje de programación y las bibliotecas disponibles, se ha vuelto mucho más simple. Una de las alternativas a la clase `Date` de TypeScript es utilizar la biblioteca Moment.js que ofrece una sintaxis más intuitiva para manejar fechas y horas.

Para obtener la fecha y hora actual en una zona horaria específica, podemos utilizar la función `toLocaleString()` y pasar como parámetro el código de la zona horaria deseada. Por ejemplo:

```TypeScript
console.log(currentDate.toLocaleString('en-US', { timeZone: 'America/New_York' }));
```
```
Salida esperada: Fri, Dec 10, 2021, 10:35 AM
```

Otra forma de obtener la fecha y hora actual en TypeScript es utilizar la clase `DatePipe` de Angular, que ofrece un formato más fácil de leer y personalizar.

Conclusión

Obtener la fecha y hora actual es una tarea importante para los programadores, y en TypeScript se ha vuelto mucho más sencillo gracias a la clase `Date` y las bibliotecas disponibles. Además, podemos personalizar fácilmente el formato y la zona horaria de la fecha y hora para adaptarse a nuestras necesidades. ¡Ahora estás listo para trabajar con fechas y horas en tus proyectos de TypeScript!

¡Míralo también!

- Documentación oficial de la clase `Date` en TypeScript: https://www.typescriptlang.org/docs/handbook/datetime.html
- Moment.js: https://momentjs.com/
- Documentación de la función `toLocaleString()`: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/toLocaleString
- Documentación de la clase `DatePipe` en Angular: https://angular.io/api/common/DatePipe