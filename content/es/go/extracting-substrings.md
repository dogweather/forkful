---
title:                "Go: Extrayendo subcadenas"
simple_title:         "Extrayendo subcadenas"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/go/extracting-substrings.md"
---

{{< edit_this_page >}}

## ¿Por qué extraer subcadenas en Go?

Extraer subcadenas, o partes de una cadena, puede ser útil en muchas situaciones. Por ejemplo, si tiene una cadena larga y solo necesita una parte específica de ella, extraer una subcadena le permitirá manejarla de manera más eficiente.

## Cómo extraer subcadenas en Go

La forma más sencilla de extraer una subcadena en Go es usando el operador de sección `[:]`. Este operador permite especificar una sección de una cadena utilizando índices numéricos.

Por ejemplo, si tenemos una cadena llamada `cadena` y queremos extraer la letra "h" en la posición 5, podemos usar `cadena[5:6]`. El resultado será una nueva cadena que contiene solo la letra "h".

Para mostrar cómo funciona esto en código, aquí hay un ejemplo de cómo extraer una subcadena de un nombre completo y mostrar solo el apellido:

```Go
nombreCompleto := "Sofía Alonso"
apellido := nombreCompleto[6:]
fmt.Println(apellido)
```

El resultado será "Alonso" impreso en la consola.

## Profundizando en la extracción de subcadenas

Además del operador de sección, Go también ofrece la función `Substring()` para extraer subcadenas de manera más específica. Esta función toma dos argumentos: el índice de inicio y la longitud de la subcadena que se desea extraer.

Siguiendo con el ejemplo anterior, si queremos extraer solo "fia" de "Sofía", podemos usar `Substring(2,3)`, lo que significa que queremos comenzar en el tercer carácter (índice 2) y extraer una subcadena de 3 caracteres.

```Go
nombreCompleto := "Sofía Alonso"
subcadena := nombreCompleto.Substring(2,3)
fmt.Println(subcadena)
```

El resultado será "fia" impreso en la consola.

## Ver también

- [Documentación oficial de Go sobre el operador de sección](https://golang.org/ref/spec#Slice_expressions)
- [Documentación oficial de Go sobre la función Substring()](https://golang.org/pkg/strings/#Substring)
- [Ejemplos de uso de subcadenas en Go](https://gobyexample.com/slices)