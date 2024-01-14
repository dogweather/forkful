---
title:                "Gleam: Utilizando expresiones regulares"
simple_title:         "Utilizando expresiones regulares"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/gleam/using-regular-expressions.md"
---

{{< edit_this_page >}}

# Por qué usar expresiones regulares en Gleam

Si alguna vez has intentado buscar y manipular patrones de texto en un archivo o un conjunto de datos, probablemente has encontrado la tarea bastante tediosa y complicada. Aquí es donde entran en juego las expresiones regulares en Gleam. Con su capacidad de buscar y manipular patrones de texto de manera eficiente, las expresiones regulares son una herramienta valiosa que puede ahorrar tiempo y esfuerzo en la programación.

## Cómo utilizar expresiones regulares en Gleam

Para utilizar expresiones regulares en Gleam, primero debes importar el módulo `Std.Regex` en tu código. Luego, puedes utilizar la función `Regex.match` para buscar un patrón específico en una cadena de texto. Por ejemplo:

```Gleam
import Std.Regex

texto = "Hola, mi nombre es Juan"

match texto {
  Just("Juan") -> "¡Hola Juan!"
  _ -> "No se encontró el nombre"
}
```

En este código, utilizamos la función `match` para buscar un patrón que coincida con la cadena "Juan". Si se encuentra una coincidencia, se imprimirá el mensaje "¡Hola Juan!", de lo contrario, se imprimirá "No se encontró el nombre". Este es solo un ejemplo sencillo, pero las expresiones regulares en Gleam también pueden ser utilizadas para tareas más complejas y avanzadas.

## Profundizando en las expresiones regulares

Las expresiones regulares en Gleam se basan en la implementación de la biblioteca "regex" de Rust, lo que significa que comparten el mismo poder y funcionalidad. En resumen, las expresiones regulares son patrones predefinidos que se utilizan para buscar y manipular cadenas de texto. Esto puede incluir la búsqueda de palabras específicas, patrones de formato de fechas, o incluso la eliminación de ciertos caracteres de una cadena.

Además, las expresiones regulares en Gleam también admiten una amplia gama de métodos y opciones que pueden ajustarse para adaptarse a tus necesidades específicas. Por ejemplo, puedes utilizar expresiones regulares con la función `match_all` para buscar todas las posibles coincidencias en una cadena de texto en lugar de solo la primera.

# Ver también

- Documentación oficial de expresiones regulares en Gleam: https://gleam.run/documentation/tour/regular-expressions/
- Tutorial de expresiones regulares en Gleam: https://www.tutorialspoint.com/gleam/gleam_regular_expressions.htm
- Ejemplos de uso de expresiones regulares en Gleam: https://github.com/mflorence99/gleam-regex-examples