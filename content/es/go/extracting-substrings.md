---
title:                "Extrayendo subcadenas"
html_title:           "Go: Extrayendo subcadenas"
simple_title:         "Extrayendo subcadenas"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/go/extracting-substrings.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

Extraer subcadenas es un proceso en el que los programadores utilizan una parte de una cadena de texto más grande. Esta acción se realiza por varias razones, como manipular una cadena de texto para su uso en una aplicación, o para buscar y analizar ciertas secciones de información.

## Cómo:

Aquí hay un ejemplo de cómo extraer una subcadena en Go:

```Go
str := "Esta es una cadena de texto más grande"
// Extrayendo la subcadena desde el índice 11 hasta el final
subStr := str[11:]
fmt.Println(subStr) // Imprime "cadena de texto más grande"
```

Aquí hay otro ejemplo, esta vez extrayendo una subcadena de una posición específica hasta otra:

```Go
str := "Hola mundo!"
// Extrayendo la subcadena desde el índice 5 hasta el índice 9
subStr := str[5:9]
fmt.Println(subStr) // Imprime "mun"
```

## Profundizando:

Extraer subcadenas se ha vuelto una práctica común en la programación debido a su utilidad en varios casos de uso. Se puede utilizar para crear secciones de texto más pequeñas que sean más fáciles de manejar y analizar. Además de Go, otros lenguajes de programación como Java y Python también tienen métodos para extraer subcadenas.

## Vea También:

- [Documentación de Go sobre la extracción de subcadenas](https://golang.org/ref/spec#Slice_expressions)
- [Información sobre la manipulación de cadenas en Java](https://www.tutorialspoint.com/java/java_string_substring.htm)
- [Ejemplos de extracción de subcadenas en Python](https://www.geeksforgeeks.org/python-string-slicing/#slicing-in-python)