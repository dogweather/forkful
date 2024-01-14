---
title:                "Clojure: Trabajando con yaml"
simple_title:         "Trabajando con yaml"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/clojure/working-with-yaml.md"
---

{{< edit_this_page >}}

## ¿Por qué trabajar con YAML en Clojure?

Trabajar con YAML en Clojure puede facilitar el manejo de archivos de configuración y datos estructurados. Además, YAML es un lenguaje sencillo y legible que se integra bien con Clojure.

## Cómo empezar con YAML en Clojure

Para comenzar a trabajar con YAML en Clojure, necesitarás la biblioteca de Clojure llamada "data.yaml". Puedes incluirlo en tu proyecto agregando la siguiente dependencia en tu archivo "project.clj":

```Clojure
[org.clojure/data.yaml "1.0.0"]
```

Una vez agregada la dependencia, puedes importar la biblioteca en tus archivos de Clojure de la siguiente manera:

```Clojure
(ns mi-proyecto.core
  (:require [clojure.data.yaml :as yaml]
            [clojure.java.io :as io]))
```

A continuación, puedes utilizar la función "(yaml/read ...)" para leer un archivo YAML y convertirlo en una estructura de datos en Clojure:

```Clojure
(def datos (yaml/read (io/file "ejemplo.yml")))
; datos es ahora una estructura de datos en Clojure que contiene la información del archivo YAML
```

También puedes utilizar la función "(yaml/write ...)" para escribir una estructura de datos en Clojure en un archivo YAML:

```Clojure
(yaml/write (io/file "datos.yml") datos)
; esto creará un archivo YAML llamado "datos.yml" con la información de la estructura de datos
```

## Profundizando en el uso de YAML en Clojure

Además de leer y escribir archivos YAML, también puedes manipular la estructura de datos resultante en Clojure. Por ejemplo, puedes utilizar la función "get-in" para acceder a valores específicos dentro de la estructura de datos:

```Clojure
(get-in datos [:propiedad1 :propiedad2])
; esto devolverá el valor de la propiedad2 dentro de propiedad1 en la estructura de datos
```

También puedes utilizar "assoc-in" para agregar o actualizar valores en la estructura de datos:

```Clojure
(assoc-in datos [:propiedad1 :propiedad2] "nuevo valor")
; esto actualizará el valor de la propiedad2 dentro de propiedad1 en la estructura de datos
```

Existen muchas más funciones y formas de trabajar con YAML en Clojure, así que asegúrate de revisar la documentación para obtener más información y ejemplos.

## Ver también

- Documentación de la biblioteca "data.yaml": https://clojure.github.io/data.yaml/
- Tutorial sobre YAML en Clojure: https://www.braveclojure.com/consuming-api-data/#Reading_API_Data_in_the_Clojure_CRLF_CRLF_code