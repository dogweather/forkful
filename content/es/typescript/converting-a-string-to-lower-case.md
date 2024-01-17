---
title:                "Convirtiendo una cadena a minúsculas"
html_title:           "TypeScript: Convirtiendo una cadena a minúsculas"
simple_title:         "Convirtiendo una cadena a minúsculas"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/typescript/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Qué & Por qué?
Convertir una cadena de texto a minúsculas consiste en cambiar todas las letras mayúsculas de una cadena de texto por sus correspondientes minúsculas. Los programadores suelen hacerlo para normalizar y estandarizar los datos, ya que en algunos casos es necesario que las cadenas estén en el mismo formato para poder compararlas o procesarlas correctamente.

## Cómo hacerlo:
```TypeScript
const cadena = "EJEMPLO DE CADENA";
const cadenaMinuscula = cadena.toLowerCase();

console.log(cadenaMinuscula); // Salida: "ejemplo de cadena"
```

## Profundizando:
Existen varias formas de convertir una cadena a minúsculas, como por ejemplo utilizando el método `toLowerCase()` o mediante el uso del método `replace()` combinado con expresiones regulares. También es importante tener en cuenta que este proceso puede afectar a los caracteres especiales de algunos idiomas, por lo que es necesario considerar la codificación y el idioma de la cadena.

En cuanto al rendimiento, convertir una cadena a minúsculas puede ser un proceso relativamente costoso, ya que cada carácter debe ser evaluado y cambiado. Por lo tanto, es importante tener en cuenta el tamaño de las cadenas que se están procesando para no afectar negativamente el rendimiento de una aplicación.

## Ver también:
- [Método toLowerCase() en la documentación de TypeScript](https://www.typescriptlang.org/docs/handbook/strings.html#string-operations)
- [Alternativas para convertir cadenas a minúsculas en TypeScript](https://www.codecademy.com/articles/alternatives-to-uppercase-string)