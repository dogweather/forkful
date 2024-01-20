---
title:                "Encontrando la longitud de una cadena"
html_title:           "Arduino: Encontrando la longitud de una cadena"
simple_title:         "Encontrando la longitud de una cadena"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/gleam/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## ¿Qué y Por qué?

Encontrar la longitud de una cadena ('string') significa contar cuántos caracteres tiene la cadena. Los programadores lo hacen para manipular datos y controlar el flujo de un programa, por ejemplo, validar la entrada del usuario.

## Cómo hacerlo:

En Gleam, podemos usar la función `size` para encontrar la longitud de una cadena. Aquí está el código y la salida esperada:

```gleam
import gleam/string 

fn main() {
    let cadena = "Hola Mundo"
    string.size(cadena)
}
// Output: 10
```

En este caso, "Hola Mundo" tiene 10 caracteres, por lo que la salida será 10.

## Análisis Detallado:

Históricamente, la funcion `size` se utiliza en muchos lenguajes de programación, como Gleam. Este puede variar en nombre y funcionalidad, pero la idea básica es la misma: contar caracteres en una cadena. 

Alternativa a `size`, podríamos escribir nuestra propia función para calcular la longitud de una cadena. Sin embargo, `size` es más eficiente ya que está optimizado en el nivel del lenguaje de programación.

La implementación de `size` en Gleam simplemente recorre la cadena y cuenta cada carácter. Tenga en cuenta que algunos caracteres, como emojis, se consideran un solo carácter a pesar de que pueden consistir en más de un byte.

## Ver También:

Para más información sobre cadenas en Gleam, puedes visitar los siguientes enlaces:

2. [Gleam Programming Language Overview](https://gleam.run/)

Recuerda que la programación es un viaje de aprendizaje constante. ¡No tengas miedo de experimentar con nuevas funciones y enfoques!