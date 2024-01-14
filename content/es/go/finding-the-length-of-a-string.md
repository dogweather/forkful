---
title:                "Go: Encontrando la longitud de una cadena"
simple_title:         "Encontrando la longitud de una cadena"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/go/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## ¿Por qué?

Cada programador ha encontrado la situación en la que necesita conocer la longitud de una cadena de texto. Ya sea para validar un input del usuario, realizar operaciones específicas o simplemente para tener una mejor comprensión de los datos con los que se está trabajando. En este artículo, exploraremos cómo encontrar la longitud de una cadena en el lenguaje de programación Go.

## Cómo hacerlo

Para encontrar la longitud de una cadena en Go, podemos utilizar la función `len()` que acepta un argumento de tipo `string` y devuelve el número de caracteres en la cadena. Veamos un ejemplo:

```Go
cadena := "¡Hola Mundo!"
longitud := len(cadena)

fmt.Println(longitud) // Output: 12
```

En este ejemplo, definimos una variable `cadena` con una frase y luego usamos la función `len()` para guardar la longitud de la cadena en una variable `longitud`. Al imprimir esta variable, obtenemos el resultado de 12, que es el número de caracteres en la cadena "¡Hola Mundo!".

También podemos utilizar la función `len()` directamente en un input del usuario para validar su longitud. Por ejemplo, si queremos asegurarnos que el nombre ingresado por el usuario no excede cierta cantidad de caracteres, podemos hacer lo siguiente:

```Go
fmt.Println("Ingrese su nombre:")
nombre := ""

for len(nombre) > 20 {
	fmt.Println("Su nombre es demasiado largo. Inténtelo nuevamente.")
	nombre = ""
	fmt.Scan(&nombre)
}

fmt.Println("¡Hola", nombre, "bienvenido!")
```

En este caso, utilizamos un loop `for` para pedirle al usuario que ingrese su nombre hasta que la longitud de la cadena no sea mayor a 20 caracteres. Si el usuario ingresa un nombre más largo, el loop se repetirá hasta que ingrese un nombre válido.

## Profundizando

Mientras que la función `len()` de Go es bastante sencilla y útil para encontrar la longitud de una cadena, es importante tener en cuenta que en Go, una cadena no es simplemente un arreglo de caracteres. En realidad, una cadena es un tipo de dato complejo que consta de un código de bytes y un número de caracteres. Por lo tanto, si necesitas conocer el número de bytes de una cadena en lugar de su número de caracteres, puedes utilizar la función `utf8.RuneCountInString()`. Esta función cuenta el número de caracteres UTF-8 en una cadena y no solo el número de bytes como lo hace la función `len()`.

## Ver también

- [Documentación oficial de Go sobre la función `len()`](https://golang.org/pkg/builtin/#len)
- [Documentación oficial de Go sobre la función `utf8.RuneCountInString()`](https://golang.org/pkg/unicode/utf8/#RuneCountInString)
- [Tutorial de strings en Go](https://www.golangprograms.com/go-language/strings.html)