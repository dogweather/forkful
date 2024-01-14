---
title:    "Go: Encontrar la longitud de una cadena."
keywords: ["Go"]
---

{{< edit_this_page >}}

## ¿Por qué deberías aprender a encontrar la longitud de una cadena?

En la programación, a menudo nos encontramos con la necesidad de saber la longitud de una cadena de caracteres. Este conocimiento es especialmente útil en la manipulación de datos y en la creación de algoritmos eficientes. Aprender cómo encontrar la longitud de una cadena en Go te permitirá mejorar tus habilidades y desarrollar mejores soluciones en tus proyectos.

## Cómo hacerlo

La longitud de una cadena en Go se puede encontrar utilizando la función `len()`. Esta función toma como argumento una cadena y devuelve el número de caracteres que contiene. Veamos un ejemplo:

```Go
package main

import "fmt"

func main() {
    cadena := "¡Hola, mundo!"
    longitud := len(cadena)
    fmt.Println("La longitud de la cadena es:", longitud)
}
```

En este ejemplo, hemos creado una variable `cadena` con el valor "¡Hola, mundo!" y luego hemos utilizado la función `len()` para encontrar su longitud. El resultado de este programa será:

```
La longitud de la cadena es: 13
```

También es importante tener en cuenta que la función `len()` también puede ser utilizada en otras estructuras de datos como slices y arrays.

## Profundizando

Entender cómo funciona la función `len()` en Go puede ser especialmente útil si deseas crear tus propias funciones para encontrar la longitud de una cadena. Por ejemplo, puedes crear una función que cuente el número de caracteres en una cadena excluyendo los espacios en blanco. Esto se puede lograr utilizando un ciclo `for` y la función `rune` para iterar sobre cada caracter de la cadena.

Otra cosa importante a tener en cuenta es que la función `len()` no devuelve el número de palabras en una cadena, sino el número de caracteres. Por lo tanto, si deseas encontrar el número de palabras en una cadena, tendrás que realizar algunos pasos adicionales, como separar la cadena en palabras utilizando el método `strings.Split()` y luego contar el número de elementos en el slice resultante.

Conocer a fondo cómo funciona la función `len()` en Go te permitirá utilizarla de manera más efectiva en tus programas y crear soluciones más eficientes.

## Ver también

- Documentación oficial de Go sobre la función `len()`: https://golang.org/pkg/builtin/#len
- Tutorial sobre cómo encontrar la longitud de una cadena en Go: https://www.callicoder.com/golang-find-string-length/
- Ejemplos prácticos de cómo utilizar la función `len()` en Go: https://www.golangprograms.com/go-program-to-find-length-of-the-string.html