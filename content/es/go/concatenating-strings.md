---
title:                "Go: Concatenando cadenas"
simple_title:         "Concatenando cadenas"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/go/concatenating-strings.md"
---

{{< edit_this_page >}}

## Por qué

Concatenar strings es una habilidad fundamental en programación y es especialmente útil cuando se trabaja con texto. Permite combinar varias cadenas de caracteres para crear una sola, lo que puede ser útil para tareas como la impresión de mensajes o la creación de URLs.

## Cómo

La concatenación de strings en Go es simple y se puede realizar de varias formas. Una opción es utilizar el operador `+` para unir dos strings:

```Go
nombre := "Juan"
apellido := "Pérez"
nombreCompleto := nombre + " " + apellido
fmt.Println(nombreCompleto) // salida: Juan Pérez
```

Otra opción es utilizar la función `fmt.Sprintf()` que devuelve una cadena formateada con los valores especificados:

```Go
edad := 25
descripcion := fmt.Sprintf("Tengo %d años", edad)
fmt.Println(descripcion) // salida: Tengo 25 años
```

También se puede utilizar `strings.Join()` para unir un arreglo de strings en una sola cadena:

```Go
canciones := []string{"Despacito", "La Bamba", "Bailando"}
playlist := strings.Join(canciones, ", ")
fmt.Println(playlist) // salida: Despacito, La Bamba, Bailando
```

## Un poco más profundo

Es importante tener en cuenta que la concatenación de strings en Go crea una nueva cadena cada vez que se realiza. Esto puede no ser un problema con pequeñas cadenas, pero puede afectar al rendimiento en operaciones con strings más grandes.

También es importante considerar el uso de la función `bytes.Buffer` para evitar la creación constante de nuevas cadenas en el proceso de concatenación. Esta función crea un búfer de bytes que se puede utilizar para agregar strings sin crear nuevas cadenas en cada operación.

## Ver también

- [La documentación oficial de strings en Go](https://golang.org/pkg/strings/)
- [Tips para mejorar el performance en cadenas en Go](https://kgrz.io/strings-build-up-grpc-performance.html)
- [El operador `+=` en Go](https://www.geeksforgeeks.org/golang-short-assignment-operator-variable-set-value/)