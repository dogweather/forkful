---
title:                "Trabajando con yaml"
html_title:           "Elm: Trabajando con yaml"
simple_title:         "Trabajando con yaml"
programming_language: "Elm"
category:             "Elm"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elm/working-with-yaml.md"
---

{{< edit_this_page >}}

Qué es YAML y por qué los programadores lo utilizan?

YAML es un formato de serialización de datos que permite a los programadores estructurar y almacenar información de manera legible tanto para máquinas como para humanos. Los programadores utilizan YAML porque es fácil de leer y escribir, es flexible y se puede integrar fácilmente con otros lenguajes de programación.

Cómo:

Ejemplo de código en Elm:

```Elm
--Definir un diccionario en YAML
frutas:
  - manzana
  - plátano
  - fresa
  - piña

--Parsear y acceder a los datos en Elm
case decodeString decodeDictConfig yamlData of
  Ok dict ->
    dict
      |> Dict.get "frutas"
      |> Maybe.map (List.map toJsString >> Array.fromList) 
  Err err ->
    --manejar errores

--Resultado en Elm
Just [ "manzana", "plátano", "fresa", "piña" ]
```

Deep Dive:

YAML fue creado en 2001 como un formato de serialización alternativo a JSON y XML. Aunque es muy similar a JSON, YAML es más amigable para los humanos y ofrece más opciones de estructuración de datos. Algunas alternativas a YAML son TOML, XML y JSON.

Ver también:

- Documentación oficial de YAML: https://yaml.org/
- Ejemplos de código en Elm utilizando YAML: https://github.com/avh4/elm-yaml
- Ejemplos de código en otros lenguajes utilizando YAML: https://yaml.org/#examples