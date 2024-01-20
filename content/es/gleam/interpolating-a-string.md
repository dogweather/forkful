---
title:                "Interpolando una cadena de texto"
html_title:           "Haskell: Interpolando una cadena de texto"
simple_title:         "Interpolando una cadena de texto"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/gleam/interpolating-a-string.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?

La interpolación de cadenas es simplemente la inserción de datos en una cadena de texto para ayudarnos a componer cuerdas de manera más lógica y expresiva. Los programadores lo hacen para inyectar variables en una cadena para una mejor legibilidad y para hacer el código más limpio y fácil de mantener.

## ¿Cómo hacerlo?

A continuación, se muestran algunos ejemplos de cómo utilizar la interpolación de cadenas en Gleam:

```Gleam
let nombre = "Amigo"
let saludo = "¡Hola, \(nombre)!"
io.println(saludo) // mostrará "¡Hola, Amigo!"
```

En este ejemplo, la cadena "Amigo" es interpolada en la cadena "¡Hola, \(nombre)!". El resultado de la interpolación se almacena en la variable "saludo", que luego se imprime en la pantalla.

## Buceo Profundo

En términos de contexto histórico, la interpolación de cadenas ha estado presente en lenguajes de programación por bastante tiempo. En Gleam, la interpolación de cadenas es una forma más conveniente de formatear cadenas en comparación con la concatenación tradicional.

Alternativamente, puedes optar por usar la concatenación regular en lugar de la interpolación de cadenas:

```Gleam
let nombre = "Amigo"
let saludo = "¡Hola, " ++ nombre ++ "!"
io.println(saludo) // mostrará "¡Hola, Amigo!"
```

La implementación de la interpolación de cadena en Gleam se basa, en última instancia, en una serie de funciones de concatenación, admitiendo un código más legible y de aspecto más limpio.

## Ver También

Echa un vistazo a estas fuentes relacionadas para aprender más sobre la interpolación de cadenas en Gleam:

* [Documentación Oficial de Gleam: Interpolación de Cadenas](https://gleam.run/book/tour/strings.html)
* [Blog de Gleam: Interpolación de Cadenas](https://blog.gleam.run/posts/)
* [StackOverflow: ¿Cómo hacer Interpolación de Cadenas en Gleam?](https://stackoverflow.com/questions/tagged/gleam)