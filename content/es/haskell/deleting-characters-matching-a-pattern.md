---
title:    "Haskell: Borrando caracteres que coinciden con un patrón"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## ¿Por qué eliminar caracteres que coinciden con un patrón?

Eliminar caracteres que coinciden con un patrón es una tarea común en la programación, especialmente cuando se trabaja con cadenas de texto. Esta técnica puede ser útil para limpiar datos o para crear un formato específico en una cadena. En este artículo exploraremos cómo podemos lograr esto en Haskell.

## Cómo hacerlo
Para poder eliminar caracteres que coinciden con un patrón, primero debemos entender cómo Haskell trata las cadenas de texto y cómo podemos manipularlas. A continuación, se proporciona un ejemplo de código que utiliza la función `filter` para eliminar todos los números de una cadena de texto.

```Haskell
cadena = "Hola123"
nuevaCadena = filter (not.isDigit) cadena
print nuevaCadena
```
**Output:**

```
Hola
```

En este código, la función `filter` toma como argumento una función que devuelve verdadero o falso, y luego aplica esa función a cada elemento de la cadena. En nuestro ejemplo, la función `isDigit` devuelve verdadero si el carácter es un número, y al aplicar la función `not` a esto, estamos esencialmente diciendo que queremos eliminar todos los números de la cadena.

También podemos usar la función `delete` para eliminar un carácter específico de una cadena. Por ejemplo:

```Haskell
cadena = "Hola mundo!"
nuevaCadena = delete 'o' cadena
print nuevaCadena
```
**Output:**

```
Hla mundo!
```

También podemos combinar estas funciones para eliminar múltiples caracteres que coincidan con un patrón. Por ejemplo, si queremos eliminar tanto los números como los signos de exclamación de una cadena, podemos hacerlo de la siguiente manera:

```Haskell
cadena = "Hola123 mundo!"
nuevaCadena = filter (\x -> not(isDigit x) && not(x == '!')) cadena
print nuevaCadena
```
**Output:**

```
Hola mundo
```

## Un análisis más profundo

Además de las funciones `filter` y `delete`, Haskell también ofrece otras opciones para eliminar caracteres que coinciden con un patrón. Por ejemplo, podemos usar la función `dropWhile` para eliminar todos los caracteres hasta llegar a uno que coincida con un patrón determinado. Otra opción es usar expresiones regulares con la biblioteca `Text.Regex`.

Es importante tener en cuenta que Haskell trata las cadenas de texto como listas de caracteres, por lo que muchas de las funciones y técnicas que se utilizan para trabajar con listas también pueden aplicarse a cadenas de texto.

En resumen, eliminar caracteres que coinciden con un patrón es una tarea sencilla en Haskell gracias a las funciones incorporadas y la flexibilidad del lenguaje. Con un buen entendimiento de cómo trabajar con cadenas de texto y algunas funciones útiles, podemos manipular y limpiar fácilmente los datos en nuestras aplicaciones.

## Ver también

- [Documentación oficial de Haskell](https://www.haskell.org/documentation/)
- [Tutorial de Haskell en español](https://www.paradigmadigital.com/dev/haskell-en-espanol/)
- [Ejemplos de expresiones regulares en Haskell](https://wiki.haskell.org/Regular_expressions_examples)