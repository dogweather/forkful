---
title:    "TypeScript: Extrayendo subcadenas"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/typescript/extracting-substrings.md"
---

{{< edit_this_page >}}

## Por qué

Extraer subcadenas es una técnica útil para manipular y obtener información específica de las cadenas de texto en aplicaciones de TypeScript. Al aprender a extraer subcadenas, podrás mejorar la eficiencia y precisión de tus programas.

## Como hacerlo

Para extraer una subcadena de una cadena de texto en TypeScript, puedes utilizar el método `substring()`. Este método toma dos parámetros, el índice inicial y el índice final, y devuelve una nueva cadena con la subcadena extraída.

```TypeScript
let texto = "¡Hola a todos!";
console.log(texto.substring(6, 11)); // Output: a todos
```

En el ejemplo anterior, hemos extraído la subcadena "a todos" del texto original utilizando los índices 6 y 11. Es importante tener en cuenta que el índice final no está incluido en la subcadena final.

También puedes utilizar el método `slice()` de TypeScript para extraer subcadenas. Este método también toma dos parámetros, pero en este caso, el segundo parámetro representa la longitud de la subcadena en lugar del índice final.

```TypeScript
let texto = "¡Hola a todos!";
console.log(texto.slice(6, 11)); // Output: a todos
```

Además de estos métodos, TypeScript también ofrece la posibilidad de extraer la primera o última parte de una cadena utilizando los métodos `substr()` y `substring()`.

## Profundizando

Existen varias cosas a tener en cuenta al extraer subcadenas en TypeScript. Por ejemplo, si no se especifica un segundo parámetro en los métodos `substring()` o `slice()`, la subcadena resultante será hasta el final de la cadena original. Además, si se utiliza un índice negativo, se contará desde el final de la cadena hacia atrás.

Otra cosa a tener en cuenta es que estos métodos son sensibles a mayúsculas y minúsculas, por lo que si la cadena original tiene alguna letra en mayúscula, la subcadena resultante también la tendrá. Por último, si se intenta extraer una subcadena con un índice fuera de rango, se devolverá una cadena vacía.

## Ver también

- [Documentación de TypeScript sobre el método substring()](https://www.typescriptlang.org/docs/handbook/intro.html#string)
- [Ejemplos de uso de slicing en TypeScript](https://www.tutorialsteacher.com/typescript/typescript-string)

¡Ahora estás listo para empezar a extraer subcadenas en tus proyectos en TypeScript! Con esta técnica, podrás manipular y obtener la información que necesitas de tus cadenas de texto de manera más eficiente. ¡Sigue experimentando y mejorando tu habilidad en TypeScript!