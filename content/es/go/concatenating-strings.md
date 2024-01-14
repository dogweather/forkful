---
title:    "Go: Uniendo cadenas de texto"
keywords: ["Go"]
---

{{< edit_this_page >}}

## ¿Por qué concatenar cadenas en Go?

Concatenar cadenas en Go es una forma sencilla y poderosa de combinar diferentes cadenas de texto para crear una cadena más larga y completa. Puede ser útil en situaciones como la creación de mensajes de error personalizados o la formación de una URL a partir de diferentes partes.

## Cómo hacerlo en Go

Para concatenar cadenas en Go, podemos usar el operador `+`. Veamos un ejemplo:

```Go
c1 := "Hola"
c2 := "amigo"
c3 := c1 + " " + c2
fmt.Println(c3)
```

El código anterior produce la salida: `Hola amigo`. En este ejemplo, hemos creado tres cadenas diferentes: `c1` con el valor "Hola", `c2` con el valor "amigo" y `c3` que es la concatenación de las dos cadenas anteriores, agregando un espacio entre ellas.

También podemos utilizar la función `fmt.Sprintf()` para concatenar cadenas en Go:

```Go
c1 := "¡Hola!"
c2 := "¿Qué tal?"
c3 := fmt.Sprintf("%s %s", c1, c2)
fmt.Println(c3)
```

Este código también produce la salida `¡Hola! ¿Qué tal?`, ya que hemos utilizado el formato de cadena `%s` para indicar dónde queremos que se inserten nuestras cadenas.

## Un vistazo más profundo a la concatenación de cadenas

En Go, las cadenas son inmutables, lo que significa que una vez que se crean no pueden cambiarse. Por lo tanto, cada vez que concatenamos una cadena, se está creando una nueva cadena en memoria.

Además, es importante tener en cuenta que el rendimiento puede verse afectado si concatenamos grandes cantidades de cadenas utilizando el operador `+`. En su lugar, se recomienda el uso de la función `strings.Join()` para unir una gran cantidad de cadenas.

## Ver también

- [Documentación oficial de Go sobre cadenas](https://golang.org/pkg/strings/)
- [Tutorial sobre concatenación de cadenas en Go](https://www.digitalocean.com/community/tutorials/how-to-manipulate-strings-in-go-es)
- [Ejemplos de concatenación de cadenas en Go](https://gobyexample.com/strings)