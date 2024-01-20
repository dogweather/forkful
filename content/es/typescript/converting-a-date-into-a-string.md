---
title:                "Convirtiendo una fecha en una cadena de texto"
html_title:           "C++: Convirtiendo una fecha en una cadena de texto"
simple_title:         "Convirtiendo una fecha en una cadena de texto"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/typescript/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

Convertir una fecha en una cadena implica cambiar la presentación de los datos de tiempo desde una fecha a una cadena de texto, es decir, desde el objeto JavaScript `Date` a `string`. los programadores lo hacen para simplificar la visualización y manipulación de la fecha.

## Cómo hacerlo:

Aquí te mostramos un ejemplo de cómo hacerlo en TypeScript. Usaremos el método de objeto `toISOString`.

```TypeScript 
let ahora = new Date();
let fechaComoCadena = ahora.toISOString();
console.log(fechaComoCadena);
```

Cuando se ejecute este código, obtendrás una cadena que representa la fecha y hora actual en el formato ISO 8601.

## Profundización:

El método `toISOString` ha estado presente desde ECMAScript 5, que se lanzó en 2009. Antes de eso, los programadores debían implementar manualmente las conversiones de fecha a cadena.

Hay otras formas de convertir un objeto `Date` en `string`, como `toString`, `toLocaleString`, `toLocaleDateString`, etc. Cada uno tiene configuraciones y formatos ligeramente diferentes, pero todos sirven para el mismo propósito general.

Implementar la conversión de una fecha a una cadena implica llamar al método apropiado en el objeto `Date`. Es importante tener en cuenta que estos métodos devolverán fechas y horas en la zona horaria del sistema del usuario a menos que se especifique de otra manera.

## Ver también:

Para obtener más información y ejemplos, consulta los siguientes recursos:

* Método *toISOString*: https://developer.mozilla.org/es/docs/Web/JavaScript/Referencia/Objetos_globales/Date/toISOString
* Método *toString*: https://developer.mozilla.org/es/docs/Web/JavaScript/Referencia/Objetos_globales/Date/toString 
* Método *toLocaleString*: https://developer.mozilla.org/es/docs/Web/JavaScript/Referencia/Objetos_globales/Date/toLocaleString 
* Método *toLocaleDateString*: https://developer.mozilla.org/es/docs/Web/JavaScript/Referencia/Objetos_globales/Date/toLocaleDateString