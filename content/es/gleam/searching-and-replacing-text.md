---
title:                "Gleam: Buscando y reemplazando texto"
simple_title:         "Buscando y reemplazando texto"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/gleam/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

#Por qué: Búsqueda y reemplazo de texto
La búsqueda y reemplazo de texto es una herramienta importante para cualquier programador. Con ella, puedes encontrar y modificar rápidamente cadenas de texto en grandes cantidades de código. Esto puede ahorrarte mucho tiempo y esfuerzo, especialmente en proyectos extensos.

#Cómo hacerlo:
Para realizar una búsqueda y reemplazo en Gleam, utiliza la función integrada `replace` seguida de la cadena de texto que deseas buscar y la cadena de reemplazo en una sola línea de código. Por ejemplo:

```Gleam
replace("Hola mundo", /mundo/, "programación")
// salida: "Hola programación"
```

También puedes utilizar esta función en conjunción con otras operaciones de transformación de cadenas de texto, como `map` y `filter`.

#Profundizando:
Además de la función `replace`, Gleam también ofrece otras herramientas para la búsqueda y reemplazo de texto. `replace_all` te permite reemplazar todas las apariciones de una cadena de texto en una sola vez, y `replace_nth` te permite especificar la posición de la ocurrencia que deseas reemplazar. También puedes utilizar expresiones regulares para hacer coincidir patrones más complejos y modificar cadenas de texto de manera más precisa.

En general, la búsqueda y reemplazo de texto en Gleam es intuitiva y fácil de usar. Es una herramienta valiosa para depurar y modificar tu código de manera rápida y eficiente.

#Ver también:
- Documentación oficial de Gleam sobre búsqueda y reemplazo: https://gleam.run/articles/string-search-and-replace
- Ejemplos de uso de `replace` en proyectos de Gleam: https://github.com/gleam-lang/gleam/search?q=%22replace%28%22&type=Code