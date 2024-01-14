---
title:                "Go: Encontrando la longitud de una cadena"
programming_language: "Go"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/go/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

##Por qué

La longitud de una cadena, o string, es una medida importante para cualquier programa de programación Go. Saber la longitud de una cadena es útil para realizar operaciones como la manipulación de textos y la comparación de términos. Además, conocer la longitud de una cadena puede ser útil en programas que requieren un límite de caracteres para la entrada del usuario.

Puede ser tentador simplemente adivinar la longitud de una cadena, pero eso no siempre es una opción confiable. Además, está la posibilidad de que se introduzcan strings vacíos, lo que podría dar lugar a errores. Por lo tanto, es importante saber cómo encontrar la longitud de una cadena en Go.

##Cómo hacerlo

En Go, la función `len` se utiliza para encontrar la longitud de una cadena. Esta función toma una cadena como argumento y devuelve la cantidad de caracteres que contiene. Veamos un ejemplo de cómo usarlo:

```Go
cadena := "¡Hola, mundo!"
longitud := len(cadena)
fmt.Println(longitud)

// Output: 13
```

En este ejemplo, creamos una variable `cadena` que contiene la frase "¡Hola, mundo!", y luego usamos la función `len` para encontrar su longitud. La salida en la consola será 13, ya que la frase contiene 13 caracteres, incluyendo los espacios y la coma.

También podemos usar la función `len` para encontrar la longitud de una variable numérica convertida a String. Por ejemplo:

```Go
numero := 12345
longitud := len(strconv.Itoa(numero))
fmt.Println(longitud)

// Output: 5
```

Aquí, usamos la función `strconv.Itoa` para convertir el número 12345 a string y luego usamos `len` para encontrar su longitud, que en este caso es 5.

##Profundizando

La función `len` en Go es capaz de encontrar la longitud de una variedad de tipos de datos, no solo de cadenas. Puede encontrar la longitud de un array, slice, map o channel, siempre que sea un tipo de dato "indexable". Esto significa que se puede iterar sobre él y acceder a elementos individuales.

Además, es importante tener en cuenta que la función `len` solo devuelve la cantidad de elementos indexables en una variable. Por ejemplo, si tenemos una cadena con una palabra acentuada, como "café", `len` devolverá la longitud de 4 en lugar de 5. Esto se debe a que en Unicode, la "é" se considera un solo caractér y ocupa un solo espacio en la cadena.

##Vea también

- [Documentación oficial de la función `len` en Go](https://golang.org/ref/spec#Length_and_capacity)
- [Ejemplos prácticos de uso de `len` en Go](https://yourbasic.org/golang/len-capacity-slices/)
- [Cómo encontrar la longitud de una cadena en otros lenguajes de programación](https://www.guru99.com/string-length.html)