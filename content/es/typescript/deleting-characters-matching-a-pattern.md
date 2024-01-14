---
title:    "TypeScript: Eliminando caracteres que coinciden con un patrón"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

## Por qué

Eliminar caracteres que coinciden con un patrón es una práctica común en programación para limpiar o procesar datos según ciertas reglas. También puede ser útil para validar entradas de usuario en un formulario o para realizar operaciones específicas en una cadena de texto.

## Cómo Hacerlo

Para eliminar caracteres que coinciden con un patrón en TypeScript, podemos utilizar el método `replace()` de la clase `String`. Este método acepta dos argumentos: el patrón que se desea buscar y el reemplazo que se realizará.

Dado el siguiente ejemplo de código:

```TypeScript
let frase = "¡Hola! Soy un texto de ejemplo.";
console.log(frase.replace(/¡/g, ""));
```

El resultado sería:

```
Hola! Soy un texto de ejemplo.
```

En este caso, estamos buscando el patrón `/¡/g` que representa el signo de exclamación `¡` y utilizando una cadena vacía `""` como reemplazo, logrando así eliminar todos los signos de exclamación de la frase.

También podemos utilizar otros patrones como expresiones regulares para lograr una eliminación más específica, por ejemplo:

```TypeScript
let frase = "¡Hola! ¿Cómo estás? ¡Espero que bien!";
console.log(frase.replace(/[!¿]/g, ""));
```

En este caso, estamos buscando todos los signos de exclamación `!` y de interrogación `¿` y eliminándolos de la frase, resultando en la siguiente salida:

```
Hola Cómo estás Espero que bien
```

## Profundizando

Eliminar caracteres que coinciden con un patrón puede ser muy útil cuando se trabaja con datos no estructurados o cuando se desea realizar operaciones específicas en una cadena. Además de utilizar expresiones regulares, también podemos utilizar el método `split()` para separar una cadena en varias partes utilizando un delimitador y luego unir las partes deseadas utilizando el método `join()`.

Por ejemplo, si tenemos una cadena que contiene nombres y apellidos separados por un guión `-`, podemos utilizar lo siguiente para obtener solo el primer nombre:

```TypeScript
let nombreCompleto = "Juanito-González-Pérez";
let nombre = nombreCompleto.split("-")[0];
console.log(nombre); // salida: Juanito
```

Sin embargo, es importante tener en cuenta que al eliminar caracteres que coinciden con un patrón, podemos alterar la estructura de la cadena original, por lo que es recomendable utilizarlo con precaución y tener en cuenta los posibles resultados.

## Ver también

- [Método replace() en la documentación de TypeScript](https://www.typescriptlang.org/docs/handbook/built-in-operators.html#string-replace)
- [Expresiones regulares en JavaScript - MDN](https://developer.mozilla.org/es/docs/Web/JavaScript/Guide/Regular_Expressions)
- [Método split() en la documentación de TypeScript](https://www.typescriptlang.org/docs/handbook/strings.html#split)