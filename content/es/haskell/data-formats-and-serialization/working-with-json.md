---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:23:09.877143-07:00
description: "C\xF3mo: Haskell no tiene soporte incorporado para JSON como JavaScript,\
  \ pero con la ayuda de bibliotecas de terceros como **Aeson**, manejar JSON se vuelve\u2026"
lastmod: '2024-03-13T22:44:59.139846-06:00'
model: gpt-4-0125-preview
summary: Haskell no tiene soporte incorporado para JSON como JavaScript, pero con
  la ayuda de bibliotecas de terceros como **Aeson**, manejar JSON se vuelve sencillo.
title: Trabajando con JSON
weight: 38
---

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
