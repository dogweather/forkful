---
title:    "TypeScript: Buscando y reemplazando texto"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/typescript/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Por qué
¿Alguna vez te has encontrado en la situación de tener que cambiar todas las instancias de una palabra o frase en un archivo de código? Puede ser una tarea tediosa y propensa a errores si se hace manualmente. Afortunadamente, TypeScript tiene una función incorporada que facilita la búsqueda y reemplazo de texto en tus archivos de código.

## Cómo hacerlo
Para realizar una búsqueda y reemplazo de texto en TypeScript, puedes usar el método `replace()` aplicado a una cadena de texto. Aquí hay un ejemplo de cómo podrías usarlo:

```TypeScript
let texto = "Hola, soy un texto de ejemplo";
console.log(texto.replace("hola", "adiós"));
```
**Salida:** Adiós, soy un texto de ejemplo

En este ejemplo, utilizamos el método `replace()` para buscar y reemplazar la palabra "hola" por "adiós" en la cadena de texto. Puedes usar esta función para reemplazar una palabra o varias palabras en una cadena.

También puedes utilizar expresiones regulares en el método `replace()` para hacer búsquedas más avanzadas. Por ejemplo, si quisieras reemplazar todas las letras mayúsculas en una cadena por minúsculas, puedes hacer lo siguiente en tu código TypeScript:

```TypeScript
let texto = "Hola, Soy UN TEXTO De Ejemplo";
console.log(texto.replace(/[A-Z]/g, (letra) => letra.toLowerCase()));
```
**Salida:** hola, soy un texto de ejemplo

Usamos una expresión regular para buscar todas las letras mayúsculas (indicada por `[A-Z]`) y luego las reemplazamos con su equivalente en minúsculas utilizando la función de flecha en el método `replace()`. Este es solo un ejemplo simple de cómo puedes utilizar expresiones regulares para realizar búsquedas y reemplazos más avanzados.

## Profundizando
En TypeScript, también puedes usar la función `replaceAll()` para reemplazar todas las instancias de una cadena por otra. Esta función está disponible en TypeScript 4.1 o superior, por lo que asegúrate de tener la versión correcta antes de usarla.

Otra forma de realizar búsquedas y reemplazos es utilizando la librería externa `regex-replace` que te permite realizar búsquedas y reemplazos utilizando expresiones regulares de una manera más intuitiva.

## Ver también
- [Documentación oficial de TypeScript sobre el método replace()](https://www.typescriptlang.org/docs/handbook/strings.html#search-and-replace)
- [Guía de expresiones regulares en TypeScript](https://www.regular-expressions.info/typescript.html)
- [Librería regex-replace](https://www.npmjs.com/package/regex-replace)