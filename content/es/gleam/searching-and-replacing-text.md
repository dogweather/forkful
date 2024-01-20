---
title:                "Buscando y reemplazando texto"
html_title:           "C: Buscando y reemplazando texto"
simple_title:         "Buscando y reemplazando texto"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/gleam/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## ¿Qué & Por Qué?
La búsqueda y reemplazo de texto son acciones fundamentales en programación. Los programadores las hacen para modificar el texto de una forma controlada, por ejemplo, cambiar nombres de variables, eliminar espacios en blanco, o corregir errores tipográficos.

## Cómo hacer:
A continuación, un ejemplo simple de cómo buscar y reemplazar texto en Gleam.

```Gleam
import gleam/string

let mi_cadena = "Hola Mundo!"
let nuevo_texto = string.replace(mi_cadena, "Hola", "Adiós")
```
La salida sería:

```Gleam
"Adiós Mundo!"
```

## Inmersión Profunda
La búsqueda y reemplazo de texto han sido parte integral de la programación desde su inicio. En el contexto histórico, la capacidad de buscar y reemplazar texto fue una mejora significativa en la eficiencia de la codificación.

Existen alternativas a la función `string.replace`, como utilizar expresiones regulares para un reemplazo más complejo.

En cuanto a la implementación, básicamente, `string.replace` busca todas las ocurrencias de una subcadena y las reemplaza con una nueva subcadena.

## Ver También
Para más información acerca de la búsqueda y reemplazo de texto en Gleam, y usos más complejos de `string.replace`, consulte estos enlaces:
- Documentación oficial de Gleam: [link](https://gleam.run/)
- Manual de la biblioteca de cadenas Gleam: [link](https://github.com/gleam-lang/stdlib/tree/main/src/gleam/string.gleam)
- Discusión en el foro de Gleam sobre búsqueda y reemplazo de texto: [link](https://discuss.gleam-lang.org/)