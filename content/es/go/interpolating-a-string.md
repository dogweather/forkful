---
title:                "Interpolación de una cadena"
html_title:           "Go: Interpolación de una cadena"
simple_title:         "Interpolación de una cadena"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/go/interpolating-a-string.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?
La interpolación de cadenas es una técnica común en programación que permite insertar valores variables dentro de una cadena de texto. Los programadores utilizan esta técnica para crear cadenas dinámicas que contengan información específica.

## Cómo hacerlo:
Para realizar la interpolación de cadenas en Go, primero se debe declarar la cadena con la que se desea trabajar, seguido por el símbolo de porcentaje (%) que indica dónde se insertará el valor variable. Luego, se debe usar el verbo correspondiente al tipo de valor que se desea insertar, como por ejemplo "%s" para cadenas, "%d" para enteros y "%f" para decimales.

```Go
cadena := "Hola, %s"
nombre := "Juan"
fmt.Printf(cadena, nombre)

// Output: Hola, Juan
```

## Profundizando:
La interpolación de cadenas es una técnica comúnmente usada en lenguajes de programación, que surge de la necesidad de combinar información de manera dinámica. En otros lenguajes, esta técnica se realiza a través de la concatenación de cadenas, pero en Go se prefiere esta forma más simple y eficiente.

Una alternativa a la interpolación de cadenas en Go sería el uso de la función `fmt.Sprintf()` que, al igual que `fmt.Printf()`, permite insertar valores variables en una cadena, pero retorna el resultado en lugar de imprimirlo directamente.

La implementación de la interpolación de cadenas en Go se basa en el paquete `fmt` y el uso de verbos específicos para cada tipo de valor. Además, se pueden utilizar más de un verbo en una sola cadena, y luego proveer los valores correspondientes en el orden adecuado.

## Ver también:
Para más información sobre la interpolación de cadenas en Go, se pueden consultar los siguientes recursos:

- [Documentación oficial de Go](https://golang.org/pkg/fmt/)
- [Post de blog sobre interpolación de cadenas en Go](https://blog.learngoprogramming.com/golang-sprintf-cad23e350f45)