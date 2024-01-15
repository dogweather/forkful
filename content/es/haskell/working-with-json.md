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

## Por qué

Si estás buscando una forma sencilla y eficiente de manejar datos en tus proyectos de programación, JSON es una excelente opción. Al ser un formato de datos ligero y fácil de entender, se ha vuelto muy popular en la industria.

## Cómo

Para comenzar a trabajar con JSON en Haskell, primero debemos importar la librería "Data.Aeson". A continuación, podemos crear un objeto JSON usando la sintaxis de registros de Haskell:

```Haskell
data Persona = Persona {
  nombre :: String,
  edad :: Int
} deriving (Generic, Show)

instance FromJSON Persona
instance ToJSON Persona
```

Luego, podemos usar la función `encode` para convertir nuestro objeto a una cadena de texto JSON:

```Haskell
let persona = Persona "Juan" 25
encode persona
-- Resultado: "{\"nombre\":\"Juan\",\"edad\":25}"
```

Para decodificar una cadena de texto JSON a un objeto en Haskell, podemos usar la función `decode` y asegurarnos de manejar correctamente los posibles errores:

```Haskell
let json = "{\"nombre\":\"Maria\",\"edad\":\"30\"}"
decode json :: Maybe Persona
-- Resultado: Just (Persona {nombre = "Maria", edad = 30})
```

También podemos trabajar con listas de objetos JSON, utilizando las funciones `encode` y `decode` en su versión plural (`encodeList` y `decodeList` respectivamente). Y para trabajar con archivos JSON, podemos utilizar las funciones `encodeFile` y `decodeFile`.

## Deep Dive

Si queremos trabajar con campos opcionales en nuestros objetos JSON, podemos usar la función `(.=)` de la librería "Data.Aeson". Por ejemplo, si queremos que el campo "apellido" sea opcional en nuestra definición de "Persona", podemos hacer lo siguiente:

```Haskell
data Persona = Persona {
  nombre :: String,
  edad :: Int,
  apellido :: Maybe String
} deriving (Generic, Show)

instance FromJSON Persona where
  parseJSON (Object v) = Persona
    <$> v .: "nombre"
    <*> v .: "edad"
    <*> v .:? "apellido"

instance ToJSON Persona where
  toJSON (Persona nombre edad Nothing) = object [
    "nombre" .= nombre,
    "edad" .= edad
  ]
  toJSON (Persona nombre edad (Just apellido)) = object [
    "nombre" .= nombre,
    "edad" .= edad,
    "apellido" .= apellido
  ]
```

También podemos trabajar con tipos de datos más complejos, como por ejemplo, listas y mapas. Para ello, podemos utilizar la librería "Data.HashMap.Strict" y sus funciones `fromList` y `toList`.

## Ver también

- <https://hackage.haskell.org/package/aeson>
- <https://hackage.haskell.org/package/hashmap-strict>
- <https://www.haskell.org/learn/>