---
title:                "Capitalizando una cadena de texto"
html_title:           "TypeScript: Capitalizando una cadena de texto"
simple_title:         "Capitalizando una cadena de texto"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/typescript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## ¿Qué & Por qué?

Capitalizar una cadena significa convertir la primera letra de cada palabra en mayúsculas. Los programadores lo hacen para mejorar la legibilidad y la consistencia del texto en la salida.

## Cómo hacerlo:

Aquí hay una función básica en TypeScript para capitalizar una cadena de texto:

```TypeScript
function capitalizarCadena(cadena: string): string {
    return cadena
        .toLowerCase()
        .split(' ')
        .map(palabra => palabra.charAt(0).toUpperCase() + palabra.slice(1))
        .join(' ');
}

console.log(capitalizarCadena("hola mundo"));  // Salida: "Hola Mundo"
```

La función convierte toda la cadena a minúsculas y luego divide la cadena en palabras. Luego, toma la primera letra de cada palabra, la convierte en mayúsculas y se mantiene el resto sin cambios. Finalmente, se unen las palabras en una cadena.

## Profundizando:

**Contexto histórico:** La capitalización ha existido desde los primeros idiomas escritos, donde se utilizaba para destacar nombres propios y comienzos de oraciones. La capitalización en la programación sigue el mismo principio para mejorar la legibilidad y destacar partes importantes del texto.

**Alternativas:** TypeScript no tiene una función incorporada para capitalizar una cadena. Si quisieras evitar escribir tu propia función, podrías usar una biblioteca de funciones útiles llamada Lodash. Su función `_.capitalize` convierte la primera letra de una cadena a mayúsculas y el resto a minúsculas:

```TypeScript
import _ from 'lodash';

console.log(_.capitalize('hola mundo'));  // Salida: "Hola mundo"
```

**Detalles de implementación:** Nuestra función `capitalizarCadena` utiliza varios métodos de cadena en JavaScript, que están disponibles porque TypeScript es un superconjunto de JavaScript. Esas funciones incluyen `toLowerCase`, `split`, `map`, `charAt`, `toUpperCase`, `slice` y `join`.

## Ver también:

Si buscas más profundidad, aquí te dejo algunos enlaces para aprender más acerca de las cadenas en TypeScript:

1. [Documentación oficial de TypeScript sobre cadenas][1]
2. [Métodos de cadenas en JavaScript en MDN][2]
3. [Biblioteca Lodash][3]

[1]: https://www.typescriptlang.org/docs/handbook/2/strings.html
[2]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String
[3]: https://lodash.com/docs/4.17.15