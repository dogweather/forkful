---
title:                "Analizando una fecha de una cadena"
html_title:           "TypeScript: Analizando una fecha de una cadena"
simple_title:         "Analizando una fecha de una cadena"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/typescript/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?
El análisis de fechas de una cadena es el proceso de extracción de la información de fecha y hora de una cadena de texto en un formato específico. Los programadores lo hacen para poder manipular los datos de fecha y hora en sus programas de manera más eficiente.

## Cómo realizarlo:
El lenguaje de programación TypeScript tiene una función incorporada llamada "new Date()" que permite analizar una cadena de texto y convertirla en un objeto de fecha. Aquí hay un ejemplo de cómo usar esta función:

```
TypeScript
let dateString = "12/31/2020";
let date = new Date(dateString);
console.log(date);
```

El resultado de este código sería un objeto de fecha que representa el 31 de diciembre de 2020.

También es posible especificar un formato específico para el análisis de fechas utilizando una biblioteca externa como Moment.js. Aquí hay un ejemplo de cómo hacerlo:

```
TypeScript
import moment from 'moment';

let dateString = "Thursday, 12/31/2020";
let date = moment(dateString, "dddd, MM/DD/YYYY");
console.log(date.format());
```

En este caso, el resultado sería una fecha en un formato personalizado: "2020-12-31T00:00:00-05:00".

## Profundizando:
El análisis de fechas de una cadena ha existido desde los primeros días de la informática. Antes de la llegada de la programación orientada a objetos, las fechas solían ser almacenadas como cadenas y tenían que ser convertidas manualmente para realizar operaciones matemáticas. Sin embargo, con la introducción de objetos de fecha en los lenguajes de programación, el proceso se ha vuelto mucho más eficiente.

Además de la función "new Date()" de TypeScript, hay otras formas de analizar fechas, como utilizando expresiones regulares o la biblioteca nativa "parseInt()". Sin embargo, estas opciones pueden ser más complejas y menos precisas que la función incorporada de TypeScript.

En términos de implementación, el proceso de análisis de fechas implica la separación de la cadena en sus componentes de fecha y hora y luego asignar esos valores al objeto de fecha correspondiente.

## Ver también:
- Documentación oficial de TypeScript sobre la función "new Date()": https://www.typescriptlang.org/docs/handbook/release-notes/typescript-2-1.html#new-date
- Documentación de Moment.js para especificar formatos personalizados: https://momentjs.com/docs/#/parsing/string-format/
- Tutorial de DigitalOcean sobre el análisis de fechas con Express.js: https://www.digitalocean.com/community/tutorials/how-to-parse-and-format-dates-in-javascript