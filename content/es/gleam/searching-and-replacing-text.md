---
title:                "Buscando y reemplazando texto"
date:                  2024-01-20T17:57:51.293126-07:00
model:                 gpt-4-1106-preview
simple_title:         "Buscando y reemplazando texto"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/gleam/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Qué y Por Qué?

Buscar y reemplazar texto es una tarea de edición común que permite cambiar una cadena de texto específica por otra. Los programadores la utilizan frecuentemente para corregir errores, actualizar nombres de variables o modificar datos en masa.

## Cómo:

```gleam
import gleam/string

fn main() {
  let texto = "Hola, mundo! El mundo es amplio y vasto."
  let nuevo_texto = string.replace(texto, "mundo", "universo")
  nuevo_texto
}
```

Salida esperada:

```
"Hola, universo! El universo es amplio y vasto."
```

## Profundización

Históricamente, la necesidad de buscar y reemplazar texto proviene de la edición de documentos y ha sido adaptada a la programación dada su versatilidad. En Gleam, la función `replace` de la biblioteca `string` es suficiente para casos simples. Alternativas incluyen expresiones regulares para patrones complejos y la manipulación de otros tipos de datos, como listas o registros. El rendimiento varía dependiendo del algoritmo y la estructura de datos subyacentes, pero en la mayoría de los casos, es una operación rápida y eficiente.

## Ver También

- Tutoriales de expresiones regulares en Gleam: insert_urls_to_relevant_tutorials
- Artículo sobre la eficiencia del algoritmo de búsqueda y reemplazo: insert_relevant_article_url
