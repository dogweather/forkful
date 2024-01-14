---
title:    "TypeScript: Eliminar caracteres que coinciden con un patrón."
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/typescript/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## ¿Por qué borrar caracteres que coinciden con un patrón?

A veces, al trabajar con cadenas de texto en un proyecto de TypeScript, podemos encontrarnos con la necesidad de eliminar caracteres que coinciden con un cierto patrón. Esto puede ser por una variedad de razones, como limpiar una entrada de usuario o procesar datos antes de su uso. En este artículo, te explicaremos cómo puedes lograr esto en tu código de TypeScript.

## Cómo hacerlo

Para eliminar caracteres que coinciden con un patrón en TypeScript, podemos usar el método `replace()` junto con una expresión regular. Por ejemplo, si queremos eliminar todos los caracteres no numéricos de una cadena de texto, podemos usar la siguiente expresión regular: `/[^0-9]/g`, que coincide con todo lo que no sea un dígito del 0 al 9.

```TypeScript
let cadena = "H0!l4 m$uNd0";
cadena = cadena.replace(/[^0-9]/g, "");

console.log(cadena); // output: 0140
```

Aquí, primero asignamos una cadena de texto que contiene caracteres no numéricos a una variable. Luego, usamos el método `replace()` pasando como argumento nuestra expresión regular y una cadena vacía, lo que básicamente elimina todos los caracteres que coinciden con el patrón. Finalmente, imprimimos la cadena resultante en la consola y obtenemos solo los números.

## Profundizando

En este ejemplo, usamos la expresión regular `/[^0-9]/g` para eliminar caracteres que no son números. Sin embargo, esta expresión puede ser modificada para satisfacer nuestras necesidades específicas. Por ejemplo, si queremos eliminar solo los caracteres que no son letras, podemos usar la expresión regular `/[^a-z]/ig`, que coincidirá con todo lo que no sea una letra mayúscula o minúscula.

También podemos usar el operador `^` para negar un patrón específico, lo que significa que coincidirá con todo lo que no sea ese patrón. Por ejemplo, si queremos eliminar solo los caracteres que no son ni letras ni números, podemos usar la expresión regular `/[^a-z0-9]/ig`.

## Ver también

- [Documentación de expresiones regulares en TypeScript](https://www.typescriptlang.org/docs/handbook/regular-expressions.html)
- [Guía de expresiones regulares en MDN](https://developer.mozilla.org/es/docs/Web/JavaScript/Guide/Regular_Expressions)
- [Tutorial de expresiones regulares en TypeScript](https://www.tutorialspoint.com/typescript/typescript_regular_expressions.htm)

Esperamos que este artículo te haya sido útil para aprender cómo eliminar caracteres que coinciden con un patrón en TypeScript. ¡Hasta la próxima!