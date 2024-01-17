---
title:                "Interpolación de una cadena de texto"
html_title:           "Gleam: Interpolación de una cadena de texto"
simple_title:         "Interpolación de una cadena de texto"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/gleam/interpolating-a-string.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?
 Interpolar una cadena de texto es una técnica común utilizada por programadores para combinar variables con texto en una sola cadena. Esto permite una concatenación más eficiente y legible, especialmente cuando se trabaja con grandes cantidades de datos. 

## Cómo:
El siguiente ejemplo muestra cómo interpolar una cadena de texto en Gleam: 
```Gleam
let variable = "mundo"
let cadena = s!"¡Hola {variable}!"
```
Salida:
`"¡Hola mundo!"`

También es posible interpolar múltiples variables en una sola cadena, como se muestra a continuación:
```Gleam
let nombre = "Maria"
let apellido = "Morales"
let mensaje = s!"Bienvenido, {nombre} {apellido}!"
```
Salida:
`"Bienvenido, Maria Morales!"`

## Profundizando:
La interpolación de cadenas de texto es una técnica común utilizada en muchos lenguajes de programación, incluyendo Python, Ruby y Java. También se conoce como "formato de cadena" o "concatenación de cadenas". 

En otros lenguajes, es común tener que utilizar funciones específicas para interpolar variables en una cadena, mientras que en Gleam, usar la sintaxis `s!"...{variable}..."` hace que el código sea más claro y legible. 

## Véase también:
Para más información sobre la sintaxis y el uso de la interpolación de cadenas en Gleam, se recomienda consultar la documentación oficial: [https://gleam.run/book/tutorials/interpolation.html](https://gleam.run/book/tutorials/interpolation.html).