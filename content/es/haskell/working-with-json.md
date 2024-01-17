---
title:                "Trabajando con json"
html_title:           "Haskell: Trabajando con json"
simple_title:         "Trabajando con json"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/haskell/working-with-json.md"
---

{{< edit_this_page >}}

# ¿Qué & Por qué?

Trabajar con JSON significa manejar datos estructurados en formato de texto, que se pueden intercambiar entre diferentes lenguajes de programación y aplicaciones. Los programadores utilizan JSON para almacenar y transmitir información fácilmente legible y manipulable.

# ¿Cómo hacerlo?

```Haskell
-- Importar el módulo JSON de Haskell
import Data.Aeson

-- Ejemplo de una lista de tareas en formato JSON
tareasJSON = "[{\"descripcion\": \"Hacer la compra\", \"completado\": false}, {\"descripcion\": \"Llamar al dentista\", \"completado\": true}, {\"descripcion\": \"Hacer ejercicio\", \"completado\": false}]"

-- Convertir el JSON a un valor de tipo Haskell
tareasHaskell = decode tareasJSON :: Maybe [Value]

-- Imprimir el resultado
print tareasHaskell
```

Resultado:

```
Just ["{\"descripcion\":\"Hacer la compra\",\"completado\":false}","{\"descripcion\":\"Llamar al dentista\",\"completado\":true}","{\"descripcion\":\"Hacer ejercicio\",\"completado\":false}"]
```

# Profundizando

El formato JSON fue diseñado en 2001 por Douglas Crockford, quien propuso su uso como una alternativa más ligera y sencilla al lenguaje de marcado XML. Además de su popularidad en la comunicación entre aplicaciones web, también es ampliamente utilizado en el almacenamiento de datos en bases de datos NoSQL.

Otras formas de manejar datos estructurados en Haskell incluyen el uso de bibliotecas como CSV, YAML y XML. Sin embargo, JSON se ha consolidado como una de las opciones más utilizadas debido a su simplicidad y compatibilidad con múltiples lenguajes.

En Haskell, el tipo de datos utilizado para trabajar con JSON es `Value`, definido en el módulo `Data.Aeson`. Este tipo incluye constructores como `Object`, `Array`, `String`, `Number` y `Bool`, que se pueden utilizar para acceder y manipular diferentes tipos de datos JSON.

# Ver También

- [Guía de Aeson de Haskell](https://artyom.me/aeson)
- [Introducción a JSON](https://www.json.org/json-es.html)