---
title:                "Extracción de subcadenas"
date:                  2024-01-20T17:45:46.067719-07:00
model:                 gpt-4-1106-preview
simple_title:         "Extracción de subcadenas"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/gleam/extracting-substrings.md"
---

{{< edit_this_page >}}

## Qué y Por Qué?

Extraer subcadenas significa seleccionar partes específicas de una cadena de texto. Los programadores lo hacen para manipular, analizar o alterar datos de manera más precisa.

## Cómo hacerlo:

En Gleam, puedes extraer subcadenas utilizando la función `slice` del módulo `gleam/string`. Aquí tienes un ejemplo:

```gleam
import gleam/string

pub fn main() {
  let texto = "¡Hola mundo!"
  let substring = string.slice(start: 1, end: 5, from: texto)
  substring
}
```

Salida de muestra:

```
"ola "
```

Este código toma la subcadena desde el índice 1 hasta el 5 de la cadena `"¡Hola mundo!"`

## Análisis Profundo:

La extracción de subcadenas no es algo nuevo y ha sido una herramienta clave en la manipulación de texto desde los primeros días de la programación. En Gleam, la extracción se maneja con cuidado para evitar errores comunes, como los índices fuera de rango. Alternativas en otros lenguajes incluyen métodos como `substring`, `substr` o el uso de operaciones de array/slides. Gleam, al mantenerse funcional y siguiendo el principio de "explícito sobre implícito", proporciona una API clara y robusta para trabajar con cadenas de forma segura.
