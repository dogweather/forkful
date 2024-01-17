---
title:                "Encontrar la longitud de una cadena"
html_title:           "Go: Encontrar la longitud de una cadena"
simple_title:         "Encontrar la longitud de una cadena"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/go/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?
En programación, encontrar la longitud de una cadena de texto es simplemente contar el número de caracteres que contiene. Los programadores suelen hacer esto para realizar ciertas operaciones en cadenas, como validar la entrada de un usuario o manipular texto en un programa.

## Cómo:
Para encontrar la longitud de una cadena en Go, puedes usar la función incorporada `len()`. Esta función toma cualquier tipo de dato y devuelve su longitud en bytes. Aquí hay un ejemplo de cómo usarla:

```
cadena := "¡Hola, mundo!"
longitud := len(cadena)
fmt.Println(longitud)
```
¡El resultado sería 13! Puedes probar con diferentes cadenas para ver cómo funciona.

## Inmersión profunda:
Esta tarea aparentemente simple puede tener implicaciones históricas interesantes. En los primeros días de la informática, los programas debían ser escritos en lenguaje ensamblador, lo que significaba que los programadores tenían que contar manualmente el número de caracteres en una cadena antes de pasarla al programa. Con el tiempo, se desarrollaron mejores lenguajes de programación que incluían esta función incorporada para facilitar el trabajo de los programadores.

En Go, también puedes usar la función `cap()` para encontrar la capacidad de una cadena. Esto te dará el número de bytes que el compilador ha reservado para la cadena en la memoria. Sin embargo, ten en cuenta que esto puede no ser lo mismo que su longitud real en bytes, ya que el compilador puede haber reservado más memoria para la cadena de lo que necesitas.

## Ver también:
Enlaces útiles para aprender más sobre Go y encontrar la longitud de cadenas:
- La documentación de Go sobre la función `len()`: https://golang.org/pkg/builtin/#len
- Un tutorial sobre cómo usar `len()` en Go: https://www.digitalocean.com/community/tutorials/how-to-use-the-len-function-in-go
- Una discusión interesante sobre las diferencias entre `len()` y `cap()`: https://www.calhoun.io/go-cap-vs-len/