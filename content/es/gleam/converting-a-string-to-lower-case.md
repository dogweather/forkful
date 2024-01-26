---
title:                "Conversión de una cadena de texto a minúsculas"
date:                  2024-01-20T17:38:27.526301-07:00
model:                 gpt-4-1106-preview
simple_title:         "Conversión de una cadena de texto a minúsculas"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/gleam/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Qué y Por Qué?
Convertir una cadena a minúsculas significa cambiar todas las letras mayúsculas de un texto a su equivalente en minúsculas. Esto se hace para uniformizar los datos, facilitar comparaciones de texto y mejorar la consistencia.

## Cómo hacerlo:
En Gleam, puedes usar la función `string.lowercase` para convertir una cadena a minúsculas. Aquí tienes un ejemplo sencillo:

```gleam
import gleam/string

fn main() {
  let texto = "¡Hola, Mundo!"
  let texto_en_minusculas = string.lowercase(texto)
  texto_en_minusculas
}
```

Output esperado:

```
"¡hola, mundo!"
```

## Profundizando
Históricamente, convertir texto a minúsculas ha sido una tarea común en la programación porque facilita la manipulación y comparación de cadenas. Especialmente en contextos donde el formato y el uso de mayúsculas puede ser inconsistente, normalizar a minúsculas permite comparaciones más efectivas, como en los sistemas de búsqueda o al ordenar palabras por orden alfabético.

Alternativas para la conversión de mayúsculas a minúsculas varían dependiendo del lenguaje de programación, pero generalmente se tiene una función dedicada para ello, como `toLowerCase()` en JavaScript o `lower()` en Python.

En cuanto a la implementación, las funciones para convertir a minúsculas manejan típicamente mapeos de caracteres Unicode, atendiendo a la complejidad de diferentes sistemas de escritura que van más allá del alfabeto inglés estándar.

## Ver También
- Documentación de Gleam: [string.uppercase](https://hexdocs.pm/gleam_stdlib/gleam/string.html#lowercase/1) para convertir texto a mayúsculas.
- Unicode Case Folding: [Casos especiales y consideraciones](https://www.unicode.org/reports/tr21/tr21-5.html) para comprender mejor cómo se manejan las conversiones de mayúsculas y minúsculas en un contexto Unicode.
- Comparaciones de cadenas en distintos lenguajes de programación: Un artículo que muestra cómo diferentes lenguajes manejan las transformaciones y comparaciones de texto.
