---
title:                "Capitalizando una cadena de texto"
html_title:           "Arduino: Capitalizando una cadena de texto"
simple_title:         "Capitalizando una cadena de texto"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/typescript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?

Capitalizar una cadena de texto significa convertir la primera letra en mayúscula. Los programadores a menudo lo hacen para asegurar que los nombres propios o los títulos se muestren correctamente según las normas gramaticales.

## Cómo hacerlo:

Para capitalizar una cadena en TypeScript, puedes crear una función simple que tome la primera letra, la convierta en mayúscula y la concatene con el resto de la cadena.

```TypeScript
function capitalizar(cadena: string): string {
  return cadena.charAt(0).toUpperCase() + cadena.slice(1);
}

// Muestra de uso:
const titulo = "hola mundo";
console.log(capitalizar(titulo)); // Salida: Hola mundo
```

Si necesitas capitalizar cada palabra de una frase, aquí tienes otro ejemplo:

```TypeScript
function capitalizarPalabras(frase: string): string {
  return frase
    .split(' ')
    .map(palabra => palabra.charAt(0).toUpperCase() + palabra.slice(1))
    .join(' ');
}

// Muestra de uso:
const frase = "bienvenidos a TypeScript";
console.log(capitalizarPalabras(frase)); // Salida: Bienvenidos A TypeScript
```

## Profundizando

Capitalizar una cadena es un procedimiento común. En lenguajes como Python, hay métodos incorporados, como `.title()` y `.capitalize()`, pero TypeScript, al ser un superconjunto de JavaScript, no tiene estos métodos predefinidos para strings, así que los creamos.

¿Existen alternativas? Claro, algunas bibliotecas como Lodash tienen funciones como `_.capitalize`, pero para algo tan simple, una función personalizada es suficiente y evita dependencias externas.

En lo que respecta a detalles de implementación, ten en cuenta que la función `charAt()` puede devolver un string vacío si el index no existe, mientras que acceder a un carácter con `[0]` podría devolver `undefined`. Por eso, `charAt()` es más seguro para esta operación.

Además, si estás trabajando con localización o i18n, considera las reglas de capitalización de diferentes idiomas. No todos capitalizan de la misma manera y podrías necesitar una solución más compleja.

## Véase También

Para profundizar más, puedes revisar los siguientes enlaces:

- Documentación oficial de TypeScript: [TypeScript Lang](https://www.typescriptlang.org/)
- Librería Lodash para trabajar con strings y otros: [Lodash](https://lodash.com/)
- Fundamentos de JavaScript, ya que TypeScript se basa en JavaScript: [MDN Web Docs](https://developer.mozilla.org/es/docs/Web/JavaScript)