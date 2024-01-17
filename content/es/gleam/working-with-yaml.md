---
title:                "Trabajando con yaml"
html_title:           "Gleam: Trabajando con yaml"
simple_title:         "Trabajando con yaml"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/gleam/working-with-yaml.md"
---

{{< edit_this_page >}}

## Qué y por qué?

Trabajar con YAML es una forma común para que los programadores puedan almacenar y compartir datos estructurados en un formato legible para las máquinas y los humanos. Es especialmente útil para configurar aplicaciones y automatizar tareas en el desarrollo de software debido a su simplicidad y flexibilidad.

## Cómo hacerlo:

Para trabajar con YAML en Gleam, es necesario importar el módulo `yamlex` y luego utilizar la función `parse` para convertir un archivo YAML en un término de Gleam. A continuación se muestra un ejemplo de cómo se vería esto en código:

```Gleam
import gleam/yamlex

let yaml_string = "hola: mundo"
let term = yamlex.parse(yaml_string)

// El término resultante será {ok, #{hola => "mundo"}}
```

Use la función `to_yaml` para convertir un término de Gleam a un archivo YAML. Aquí hay un ejemplo:

```Gleam
import gleam/yamlex

let term = #{hola => "mundo"}
let yaml_string = yamlex.to_yaml(term)

// El resultado será "hola: mundo"
```

## Inmersión profunda:

YAML, que significa "Yet Another Markup Language" (Aún Otra Lenguaje de Marcado), se convirtió en un formato popular en la década de 2000 debido a su simplicidad y facilidad de uso. Algunas alternativas a YAML pueden ser JSON o XML, pero YAML se destaca por ser más legible y fácil de editar por humanos.

El módulo `yamlex` en Gleam se basa en la biblioteca de Python PyYAML para proporcionar una interfaz con la que los programadores puedan interactuar. Esto significa que tiene la misma sintaxis básica y los mismos métodos para convertir entre YAML y otros formatos de datos.

## Ver también:

Puede encontrar más información sobre YAML y su sintaxis en la documentación oficial: https://yaml.org/spec/ . También puede consultar la documentación del módulo `yamlex` de Gleam para obtener más detalles sobre su implementación: https://gleam.run/modules/gleam-yamlex/latest/ .