---
title:                "Eliminando caracteres que coinciden con un patrón"
html_title:           "Elixir: Eliminando caracteres que coinciden con un patrón"
simple_title:         "Eliminando caracteres que coinciden con un patrón"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/gleam/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## ¿Qué y Por qué?

Eliminar caracteres que coinciden con un patrón implica borrar secuencias especificas de caracteres dentro de un texto o cadena. Los programadores hacen esto para manejar mejor y manipular los datos textuales.

## Cómo hacerlo:

Aquí está un ejemplo de cómo puedes utilizar Gleam para eliminar caracteres que coinciden con un patrón.

```Gleam
import gleam/string

pub fn main(args: List(String)) {
  let texto_original = "Comandante en Jefe de la flota estelar"
  let texto_limpio = string.replace(texto_original, " ", "")
  texto_limpio
}
```

Salida de muestra:

```Gleam
"ComandanteenJefedelaflotaestelar"
```

En este ejemplo, eliminamos todos los espacios en blanco de la cadena `texto_original`, dejando intactos el resto de caracteres.

## Viaje Profundo:

Historicamente, eliminar caracteres es un truco comúnmente utilizado para procesar datos textuales y preparar entradas para aplicaciones como motores de búsqueda o análisis de texto. Como alternativa, podría usar expresiones regulares para abordar patrones más complejos o usar métodos como `slice` o `substring` si se conocen las posiciones exactas de los caracteres a eliminar. Implementar esto en Gleam es bastante sencillo gracias a su enfoque funcional y su énfasis en la tipificación estática y robusta.

## Ver También:

1. Documentación oficial de Gleam: https://gleam.run/docs/
2. API de Strings en Gleam: https://hexdocs.pm/gleam_stdlib/gleam/string.html
3. Expresiones regulares en Gleam: https://gleam.run/tour/pattern-matching/

Recuerda que la mejor forma de aprender es experimentando. Así que explora estas funciones y ve qué más puedes hacer!