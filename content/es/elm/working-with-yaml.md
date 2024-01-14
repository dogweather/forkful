---
title:                "Elm: Trabajando con yaml"
simple_title:         "Trabajando con yaml"
programming_language: "Elm"
category:             "Elm"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elm/working-with-yaml.md"
---

{{< edit_this_page >}}

## Por qué

Todos los proyectos de programación requieren algún tipo de gestión de datos. En lugar de usar un formato de datos complicado y potencialmente propenso a errores como JSON, muchos desarrolladores optan por utilizar YAML. Además de ser fácil de leer y escribir, YAML también es compatible con múltiples lenguajes de programación, incluyendo Elm.

## Cómo

Empezar a trabajar con YAML en Elm es muy sencillo. Primero, asegúrate de tener instalado el módulo `elm-yaml`, que te permitirá analizar y crear documentos YAML. A continuación, importa el módulo en tu código Elm:

```Elm
import Yaml
```

Para analizar un documento YAML existente, usa la función `Yaml.decode` y pasa como argumento una cadena de texto con el contenido del documento:

```Elm
inputText = ...
yamlData = Yaml.decode inputText
```

Si quieres crear un documento YAML nuevo, simplemente usa la función `Yaml.encode` y pasa como argumento un valor de tipo `Yaml.Value`. Por ejemplo:

```Elm
yamlValue = Yaml.encode <| Yaml.dict 
    [ ( "id", Yaml.string "1")
    , ( "name", Yaml.string "John")
    ]
```

## Profundizando

Además de la manipulación básica de datos, también puedes realizar operaciones más avanzadas con YAML en Elm. Por ejemplo, puedes fusionar dos documentos YAML usando la función `Yaml.merge`. También puedes navegar por la estructura de un documento YAML utilizando funciones como `Yaml.lookup` y `Yaml.toList`.

Si quieres aprender más sobre cómo trabajar con YAML en Elm, asegúrate de revisar la documentación del módulo `elm-yaml` y experimentar con diferentes ejemplos para familiarizarte con su sintaxis y funcionalidades.

## Ver también

- [Documentación del módulo elm-yaml](https://package.elm-lang.org/packages/jamesmacaulay/elm-yaml/latest)
- [Ejemplos de código YAML en Elm](https://github.com/dwyl/learn-elm/tree/master/yaml-data)