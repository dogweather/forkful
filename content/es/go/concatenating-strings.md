---
title:    "Go: Uniendo cadenas de texto"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/go/concatenating-strings.md"
---

{{< edit_this_page >}}

## ¿Por qué concatenar cadenas en Go?
La concatenación de cadenas es una técnica común en la programación para combinar varias cadenas en una sola. En Go, esta operación es especialmente útil para crear mensajes personalizados, formateo de salida y construcción de URLs.

## Cómo hacerlo
En Go, la concatenación de cadenas se puede hacer utilizando el operador `+`, que se utiliza para unir dos cadenas en una nueva. Veamos un ejemplo de cómo concatenar dos cadenas en Go:

```Go
nombre := "Juan"
apellido := "Pérez"
nombreCompleto := nombre + " " + apellido
fmt.Println(nombreCompleto)
```
El ejemplo de código anterior producirá la salida `Juan Pérez` al imprimir la variable `nombreCompleto`. Como se puede observar, las cadenas se pueden unir utilizando el operador `+` y se pueden incluir espacios en blanco o caracteres especiales como se desee.

También es posible concatenar más de dos cadenas en una sola línea de código, por ejemplo:

```Go
mensaje := "¡Hola, " + nombre + " " + apellido + "!"
```

Otra forma de concatenar cadenas en Go es utilizando la función `fmt.Sprintf`, que permite formatear la salida y concatenar varias cadenas. Veamos un ejemplo:

```Go
edad := 25
mensaje := fmt.Sprintf("¡Hola, %s %s! Tienes %d años.", nombre, apellido, edad)
```
Este código producirá la salida `¡Hola, Juan Pérez! Tienes 25 años.` al imprimir la variable `mensaje`.

## Profundizando en la concatenación de cadenas
En Go, las cadenas son inmutables, lo que significa que no se pueden modificar una vez creadas. Por lo tanto, al concatenar cadenas, se crea una nueva cadena en lugar de modificar la original.

Es importante tener en cuenta que la concatenación de cadenas puede ser costosa en términos de rendimiento en casos donde hay que unir muchas cadenas pequeñas en una sola. En estos casos, se recomienda utilizar el paquete `bytes` y su tipo `Buffer` para construir una cadena de manera más eficiente.

## Ver también
- [Documentación oficial de Go sobre la concatenación de cadenas](https://golang.org/doc/effective_go.html#concatenation)
- [Ejemplos de concatenación de cadenas en Go](https://www.geeksforgeeks.org/concatenation-strings-go/)
- [Paquete bytes en Go](https://golang.org/pkg/bytes/)

*Este artículo fue escrito para la comunidad de programación Go en español.*