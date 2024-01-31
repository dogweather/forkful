---
title:                "Trabajando con JSON"
date:                  2024-01-19
simple_title:         "Trabajando con JSON"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/haskell/working-with-json.md"
---

{{< edit_this_page >}}

## Qué es y por qué?

Trabajar con JSON (JavaScript Object Notation) es manejar un formato ligero de intercambio de datos, fácil para humanos leer y para máquinas procesar. Programadores lo usan por su simplicidad para transferir datos entre servidor y cliente, y porque es independiente del lenguaje de programación.

## Cómo hacerlo:

Para trabajar con JSON en Haskell, usaremos la biblioteca `aeson`.

Primero, importamos los módulos necesarios:

```Haskell
import Data.Aeson
import Data.ByteString.Lazy as B
import Data.Text
import Control.Monad
import Data.Maybe (fromJust)
```

Supongamos que tenemos el siguiente JSON:

```json
{
  "nombre": "Juan",
  "edad": 30,
  "email": "juan@example.com"
}
```

Para decodificar este JSON, definamos un tipo de dato y una instancia de `FromJSON`:

```Haskell
{-# LANGUAGE OverloadedStrings #-}
import Data.Aeson

data Persona = Persona
  { nombre :: Text
  , edad   :: Int
  , email  :: Text
  } deriving (Show)

instance FromJSON Persona where
    parseJSON = withObject "Persona" $ \v -> Persona
        <$> v .: "nombre"
        <*> v .: "edad"
        <*> v .: "email"

-- Ejemplo de cómo decodificarlo
main = do
    let jsonBytes = "{\"nombre\":\"Juan\",\"edad\":30,\"email\":\"juan@example.com\"}"
    let jsonData = eitherDecode jsonBytes :: Either String Persona
    print jsonData
```

Resultado:

```
Right (Persona {nombre = "Juan", edad = 30, email = "juan@example.com"})
```

Para codificar datos a JSON, utilizamos `toJSON`:

```Haskell
instance ToJSON Persona where
    toJSON (Persona nombre edad email) =
        object ["nombre" .= nombre, "edad" .= edad, "email" .= email]

main = do
    let persona = Persona "Juan" 30 "juan@example.com"
    B.putStrLn (encode persona)
```

Resultado:

```
{"nombre":"Juan","edad":30,"email":"juan@example.com"}
```

## Profundizando

JSON apareció en los primeros años del 2000, ideado por Douglas Crockford. A diferencia de XML, JSON es más liviano y su sintaxis está muy alineada con el código de programación, haciendo su manipulación más intuitiva.

A pesar de que `aeson` es la librería estándar en Haskell para trabajar con JSON, hay alternativas como `json` o `jsonb`. `aeson` es preferido por su rendimiento y facilidad de uso.

Detalles de implementación:
- `aeson` usa operadores como `.=` y `.:` para construir y acceder a datos en JSON.
- Con GHC (Glasgow Haskell Compiler), `aeson` puede aprovechar la extensión `OverloadedStrings` para simplificar la manipulación de texto.
- Es común el uso de `ByteString` para representar datos en bruto JSON y `Text` para cadenas de caracteres Unicode.

## Ver También

- `aeson` en Hackage: <https://hackage.haskell.org/package/aeson>
- Tutorial de JSON en Haskell: <https://artyom.me/aeson>
- Artículo de Douglas Crockford sobre JSON: <https://www.crockford.com/mckeeman.html>
- Especificación de JSON: <http://json.org/>
