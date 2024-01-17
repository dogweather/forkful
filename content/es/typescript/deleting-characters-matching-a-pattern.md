---
title:                "Eliminación de caracteres que coinciden con un patrón"
html_title:           "TypeScript: Eliminación de caracteres que coinciden con un patrón"
simple_title:         "Eliminación de caracteres que coinciden con un patrón"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/typescript/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## ¿Qué es y por qué?

Eliminar caracteres que coinciden con un patrón es una técnica comúnmente utilizada por los programadores para modificar cadenas de texto de manera eficiente. Al eliminar ciertos caracteres de una cadena, podemos simplificarla y hacerla más legible o adecuada para su uso en diferentes contextos.

## Cómo:

En TypeScript, podemos utilizar el método `replace` para eliminar caracteres que coinciden con un patrón. Por ejemplo, si queremos eliminar todos los signos de exclamación de una cadena, podríamos usar el siguiente código:

```
let texto = "¡Hola mundo!";
texto = texto.replace(/!/g, "");
console.log(texto); // Salida: ¡Hola mundo
```

En este caso, utilizamos una expresión regular dentro del método `replace` para encontrar y reemplazar todos los signos de exclamación en la cadena por una cadena vacía, lo que esencialmente los elimina.

## Profundizando:

La eliminación de caracteres que coinciden con un patrón se remonta a los primeros días de la informática, cuando los programadores tenían que lidiar con cadenas de texto largas y complejas. También es común encontrar esta técnica en otros lenguajes de programación, como JavaScript o Python.

Además de utilizar el método `replace`, también se pueden utilizar otras técnicas para eliminar caracteres que coinciden con un patrón, como usar el método `substring` o utilizar estructuras de control como `for` o `while` para recorrer la cadena y eliminar los caracteres según sea necesario.

## Ver también:

- [Documentación de TypeScript - Método replace](https://www.typescriptlang.org/docs/handbook/release-notes/typescript-1-0.html#string-replace-replacing-occurrences-of-a-substring) 
- [Tutorial de JavaScript - Manipulación de cadenas](https://www.freecodecamp.org/espanol/news/curso-de-javascript-apps-de-nuevo-diseno/lecciones/manipulacion-de-cadenas-en-javascript/)