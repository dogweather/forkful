---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:23:09.877143-07:00
description: "Trabajar con JSON (Notaci\xF3n de Objeto de JavaScript) en Haskell implica\
  \ analizar los datos JSON en tipos de Haskell y convertir los tipos de Haskell de\u2026"
lastmod: '2024-02-25T18:49:55.609898-07:00'
model: gpt-4-0125-preview
summary: "Trabajar con JSON (Notaci\xF3n de Objeto de JavaScript) en Haskell implica\
  \ analizar los datos JSON en tipos de Haskell y convertir los tipos de Haskell de\u2026"
title: Trabajando con JSON
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?
Trabajar con JSON (Notación de Objeto de JavaScript) en Haskell implica analizar los datos JSON en tipos de Haskell y convertir los tipos de Haskell de vuelta en JSON. Los programadores hacen esto para permitir que sus aplicaciones Haskell intercambien datos con servicios web o APIs sin problemas, una práctica común en el desarrollo de software moderno para el intercambio de datos multiplataforma.

## Cómo:
Haskell no tiene soporte incorporado para JSON como JavaScript, pero con la ayuda de bibliotecas de terceros como **Aeson**, manejar JSON se vuelve sencillo. Aeson proporciona funciones de alto y bajo nivel tanto para la codificación (convertir valores de Haskell a JSON) como para la decodificación (analizar JSON en valores de Haskell).

### Instalando Aeson
Primero, añade Aeson a las dependencias de tu proyecto actualizando tu archivo `.cabal` o usando Stack o Cabal directamente:

```shell
cabal update && cabal install aeson
```
o, si estás usando Stack:
```shell
stack install aeson
```

### Analizando JSON
Empecemos con un ejemplo básico de decodificación de datos JSON en un tipo de Haskell. Supongamos que tenemos el siguiente JSON que representa a una persona:

```json
{
  "name": "John Doe",
  "age": 30
}
```

Primero, define un tipo de datos Haskell correspondiente y hazlo una instancia de `FromJSON`:

```haskell
{-# LANGUAGE DeriveGeneric #-}

import GHC.Generics (Generic)
import Data.Aeson (FromJSON, decode)
import qualified Data.ByteString.Lazy as B

data Person = Person
  { name :: String
  , age :: Int
  } derivando (Generic, Show)

instance FromJSON Person

-- Función para decodificar JSON de un archivo
decodePerson :: FilePath -> IO (Maybe Person)
decodePerson filePath = do
  personJson <- B.readFile filePath
  return $ decode personJson
```
Uso:
Asumiendo que `person.json` contiene los datos JSON mostrados arriba, ejecutar:
```haskell
main :: IO ()
main = do
  maybePerson <- decodePerson "person.json"
  print maybePerson
```
Salida de muestra:
```haskell
Just (Person {name = "John Doe", age = 30})
```

### Codificando Valores Haskell como JSON
Para convertir un valor Haskell de vuelta a JSON, necesitas hacer tu tipo una instancia de `ToJSON` y luego usar `encode`.

```haskell
import Data.Aeson (ToJSON, encode)
import GHC.Generics (Generic)

-- Asumiendo el tipo Person de antes

instance ToJSON Person

encodePerson :: Person -> B.ByteString
encodePerson = encode

main :: IO ()
main = do
  let person = Person "Jane Doe" 32
  putStrLn $ show $ encodePerson person
```
Salida de muestra:
```json
{"name":"Jane Doe","age":32}
```

Estos ejemplos demuestran los conceptos básicos de trabajar con JSON en Haskell usando Aeson. Recuerda, Aeson ofrece mucho más, incluyendo reglas de análisis personalizadas, trabajar con JSON anidado complejo, y mucho más, adecuado para diversas necesidades y escenarios.
