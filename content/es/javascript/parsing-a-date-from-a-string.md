---
title:                "Analizando una fecha de una cadena"
html_title:           "Javascript: Analizando una fecha de una cadena"
simple_title:         "Analizando una fecha de una cadena"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/javascript/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

¿Qué y por qué?
Analizar una fecha de una cadena es una técnica común utilizada por programadores para convertir una cadena de texto que representa una fecha en un objeto de fecha en Javascript. Esto permite a los programadores manipular y utilizar la fecha en sus aplicaciones de manera más efectiva.

Cómo hacerlo:
```Javascript
// Creamos una cadena de fecha
var fecha = "30 de Septiembre, 2020";

// Utilizamos el método 'new Date()' y pasamos la cadena de fecha como argumento
var fechaObjeto = new Date(fecha);

// Imprimimos el objeto de fecha
console.log(fechaObjeto); // Output: Wed Sep 30 2020 00:00:00 GMT-0400 (hora de verano del Este)

// También podemos especificar el formato de la cadena de fecha utilizando el método 'Date.parse()'
var fechaOtros = Date.parse("Sep 30, 2020");

// Imprimimos la fecha en otro formato
console.log(fechaOtros); // Output: 1601419200000
```

Profundizando:
Al analizar una fecha de una cadena, se pueden utilizar diferentes métodos como 'new Date()' o 'Date.parse()' dependiendo de la necesidad del programador. Además, es importante tener en cuenta que el formato de la cadena de fecha debe ser compatible con Javascript para que la conversión sea exitosa. Si se encuentra un formato de fecha incompatible, puede ser necesario utilizar librerías externas como Moment.js para analizar la fecha correctamente.

Ver también:
- [Documentación de MDN sobre el objeto de fecha en Javascript](https://developer.mozilla.org/es/docs/Web/JavaScript/Referencia/Objetos_globales/Date)
- [Moment.js - una librería externa para manejar fechas y horas](https://momentjs.com/)