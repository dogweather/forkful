---
title:    "Go: Encontrando la longitud de una cadena"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/go/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## ¿Por qué encontrar la longitud de una cadena en Go?

En Go, como en cualquier otro lenguaje de programación, a menudo necesitamos saber la longitud de una cadena para realizar diferentes tareas como validación, manipulación de datos, entre otras. Por lo tanto, encontrar la longitud de una cadena es una habilidad básica e importante que todo programador en Go debe conocer.

## ¿Cómo hacerlo?

En Go, para encontrar la longitud de una cadena, podemos usar la función `len()` que devuelve el número de bytes en la cadena dada. A continuación, se muestra un ejemplo de código que demuestra cómo usar esta función:

```Go
cadena := "Hola, mundo!"
longitud := len(cadena) // esto devolverá el valor 13
fmt.Printf("La longitud de la cadena es %d", longitud)
```

La salida de este código será:

```Go
La longitud de la cadena es 13
```

Si queremos encontrar la longitud de una cadena en caracteres, podemos usar la función `RuneCountInString()` de la biblioteca `unicode/utf8`. Esta función devuelve el número de caracteres en una cadena.

```Go
cadena := "Hola, mundo!"
longitud := utf8.RuneCountInString(cadena) // esto devolverá el valor 12
fmt.Printf("La longitud de la cadena es %d", longitud)
```

La salida de este código será:

```Go
La longitud de la cadena es 12
```

También podemos usar la función `range` para iterar sobre la cadena y contar el número de caracteres.

## En profundidad

Ahora, profundicemos un poco más en cómo funciona la función `len()` en Go. Esta función cuenta el número de bytes de una cadena, no el número de caracteres. En Go, una cadena es una secuencia de bytes y cada carácter se representa por uno o más bytes. Por lo tanto, la longitud de una cadena depende del tipo de codificación de caracteres que se esté utilizando. Por ejemplo, en UTF-8, un carácter se puede representar por uno, dos, tres o cuatro bytes.

Ten en cuenta también que `len()` solo cuenta el número de caracteres válidos, es decir, si hay caracteres unicode inesperados, no se contarán en la longitud de la cadena. Por lo tanto, es importante saber qué tipo de caracteres se esperarán en la cadena y tener en cuenta esto al contar su longitud.

## Ver también

- [Documentación oficial de Go sobre la función len()](https://golang.org/pkg/builtin/#len)
- [Documentación oficial de Go sobre la función RuneCountInString()](https://golang.org/pkg/unicode/utf8/#RuneCountInString)
- [Artículo de blog sobre codificación de caracteres en Go](https://blog.golang.org/strings)