---
title:    "Gleam: Buscando y reemplazando texto."
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## Por qué 

¿Alguna vez te has encontrado con la tarea de reemplazar texto en un gran archivo? Puede ser una tarea abrumadora, pero con Gleam puedes hacerlo de manera sencilla y eficiente. En este artículo, te mostraremos cómo usar la función de búsqueda y reemplazo de texto en Gleam para facilitar tu trabajo.

## Cómo hacerlo 

Para realizar una búsqueda y reemplazo de texto en Gleam, primero debes importar el módulo `File` en tu archivo. Luego, usando la función `read`, puedes leer el contenido del archivo en una variable. A continuación, puedes usar la función `replace` de Gleam para buscar una cadena de texto específica y reemplazarla con otra. Finalmente, puedes escribir el contenido actualizado en el archivo utilizando la función `write`.

```Gleam
import File

let file_content = File.read("./mi_archivo.txt")

let nuevo_contenido = replace(file_content, "texto_original", "texto_nuevo")

File.write("./mi_archivo.txt", nuevo_contenido)
```

Con este código, el texto "texto_original" será reemplazado por "texto_nuevo" en el archivo `mi_archivo.txt`.

También puedes usar la función `find_and_replace` para buscar y reemplazar múltiples ocurrencias de una cadena de texto en el mismo archivo. Simplemente proporciona una lista de tuplas con las palabras a buscar y reemplazar.

```Gleam
import File

let file_content = File.read("./mi_archivo.txt")

let cambios = [
  ("texto_original", "texto_nuevo"),
  ("hola", "hola mundo"),
]

let nuevo_contenido = find_and_replace(file_content, cambios)

File.write("./mi_archivo.txt", nuevo_contenido)
```

¡Y eso es todo! Con estas sencillas funciones de Gleam, puedes realizar fácilmente búsquedas y reemplazos de texto en tus archivos.

## Profundizando 

Para aquellos que quieran ir más allá, Gleam también ofrece funciones avanzadas para realizar búsquedas y reemplazos con expresiones regulares. Puedes utilizar la función `replace_regex` para buscar y reemplazar basado en un patrón de expresión regular específico.

```Gleam
let file_content = File.read("./mi_archivo.txt")

let nuevo_contenido = replace_regex(file_content, ~r/foo(bar)+/, "foo baz")

File.write("./mi_archivo.txt", nuevo_contenido)
```

También puedes utilizar la función `find_and_replace_regex` para buscar y reemplazar múltiples ocurrencias basado en una lista de patrones de expresiones regulares.

```Gleam
let file_content = File.read("./mi_archivo.txt")

let cambios = [
  (~r/foo(bar)+/, "foo baz"),
  (~r/hola+/, "hola mundo"),
]

let nuevo_contenido = find_and_replace_regex(file_content, cambios)

File.write("./mi_archivo.txt", nuevo_contenido)
```

Con estas funciones, puedes realizar búsquedas y reemplazos más avanzados en tus archivos de manera eficiente.

## Ver también 

Para obtener más información sobre las funciones de búsqueda y reemplazo de texto en Gleam, consulta la documentación oficial en el sitio web de Gleam. También puedes explorar otros módulos y características del lenguaje para descubrir cómo puedes utilizar Gleam para simplificar tus tareas de programación.

- Documentación sobre búsquedas y reemplazos en Gleam: https://gleam.run/modules/file#replace
- Sitio web de Gleam: https://gleam.run/