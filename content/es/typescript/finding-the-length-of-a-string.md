---
title:    "TypeScript: Encontrar la longitud de una cadena"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/typescript/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## ¿Por qué?

En programación, a menudo nos encontramos con la necesidad de saber la longitud de una cadena de texto. Esto puede ser útil para una variedad de tareas, como validar entradas de usuario, dividir una cadena en partes más pequeñas o simplemente mostrar información en el formato correcto. Saber cómo encontrar la longitud de una cadena es una habilidad esencial para cualquier programador de TypeScript.

## Cómo hacerlo

```TypeScript
// Creamos una variable con una cadena de texto
let cadena: string = "¡Hola, mundo!";

// Usamos la propiedad "length" para obtener la longitud de la cadena
console.log(cadena.length);

// Output: 12
```

Aquí podemos ver que la propiedad "length", que es parte del objeto String en TypeScript, nos devuelve el número de caracteres en la cadena. También podemos utilizar esta propiedad en cadenas vacías o con espacios en blanco, y en ambos casos nos dará una longitud de 0.

```TypeScript
// Cadena vacía
let cadenaVacia: string = "";
console.log(cadenaVacia.length);

// Output: 0

// Cadena con espacios en blanco
let cadenaEspacios: string = "     ";
console.log(cadenaEspacios.length);

// Output: 5
```

También podemos utilizar la propiedad "length" en variables que contienen números, ya que TypeScript automáticamente los convierte en cadenas de texto cuando se usa la propiedad.

```TypeScript
// Variable con número
let num: number = 12345;
console.log(num.length);

// Output: 5
```

## Profundizando más

La propiedad "length" no solo nos devuelve la longitud de una cadena, sino que también se puede utilizar para acceder a caracteres específicos en una cadena. Esto se logra utilizando corchetes y un índice dentro de ellos.

```TypeScript
// Cadena de texto
let cadena: string = "Hello";

// Accedemos al caracter en la posición 0 (primer caracter)
console.log(cadena[0]);

// Output: H

// Accedemos al caracter en la posición 3 (cuarto caracter)
console.log(cadena[3]);

// Output: l
```

Además, si intentamos acceder a un índice que está fuera del rango de la cadena, TypeScript nos dará un error.

```TypeScript
let cadena: string = "Hello";
console.log(cadena[5]);

// Error: Index signature of string type can only represent a range of positive numbers
```

En resumen, la propiedad "length" es una herramienta útil para encontrar la longitud de una cadena de texto y también nos permite acceder a caracteres específicos en la cadena.

## Ver también

- [Documentación de TypeScript sobre la propiedad "length"](https://www.typescriptlang.org/docs/handbook/strings.html#string-length) 
- [Tutoriales de TypeScript en español](https://www.tutorialesprogramacionya.com/typescriptya/index.php) 
- [Ejemplos de código en TypeScript en GitHub](https://github.com/microsoft/TypeScript-Handbook/tree/master/examples)