---
title:                "Eliminando caracteres que coinciden con un patrón"
date:                  2024-01-20T17:42:03.924425-07:00
model:                 gpt-4-1106-preview
simple_title:         "Eliminando caracteres que coinciden con un patrón"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/gleam/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# Borrando caracteres por patrón en Gleam

## ¿Qué & Por qué?
Eliminar caracteres que coinciden con un patrón es como filtrar las partes indeseadas de un texto. Los programadores hacen esto para limpiar datos, validar entradas, o simplificar cadenas antes de procesarlas.

## Cómo hacerlo:
```gleam
import gleam/regex.{replace}

// Vamos a eliminar números de una cadena
fn delete_pattern(text: String) -> String {
  let pattern = regex.from_string("\\d")?
  replace(pattern, text, "")
}

// Ejemplo de uso
fn main() {
  let my_text = "Año 2021, mes 05."
  let cleaned_text = delete_pattern(my_text)
  io.debug(cleaned_text)  // Salida: "Año , mes ."
}
```

## Profundización
Históricamente, manipular cadenas ha sido crucial en la informática. En otros lenguajes se usan funciones similares como `replace` en JavaScript o `re.sub` en Python. En Gleam, la biblioteca `gleam/regex` nos permite realizar acciones similares.

Eliminar caracteres por patrón puede implementarse de diversas formas. Gleam, siendo un lenguaje de tipado estático, nos garantiza seguridad en las operaciones con expresiones regulares. Aunque las bibliotecas pueden variar, el concepto de buscar y reemplazar en cadena sigue siendo una herramienta fundamental para procesamiento de texto.

Alternativamente, si buscas velocidad y no necesitas la complejidad de las expresiones regulares, podrías utilizar funciones de cadena nativas que son más rápidas, pero menos poderosas.

## Ver además
- Documentación oficial de Gleam sobre expresiones regulares: https://hexdocs.pm/gleam_stdlib/gleam/regex/
- Tutorial interactivo de Regex para profundizar en patrones: https://regexone.com/
- Comparación de funciones de cadena en diferentes lenguajes: https://en.wikibooks.org/wiki/Algorithm_Implementation/Strings/String_searching