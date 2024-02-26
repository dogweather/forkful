---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:25:23.049022-07:00
description: "YAML, siglas de \"YAML Ain't Markup Language\" (YAML no es un lenguaje\
  \ de marcado), es un est\xE1ndar de serializaci\xF3n de datos amigable para los\
  \ humanos que\u2026"
lastmod: '2024-02-25T18:49:55.608849-07:00'
model: gpt-4-0125-preview
summary: "YAML, siglas de \"YAML Ain't Markup Language\" (YAML no es un lenguaje de\
  \ marcado), es un est\xE1ndar de serializaci\xF3n de datos amigable para los humanos\
  \ que\u2026"
title: Trabajando con YAML
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?

YAML, siglas de "YAML Ain't Markup Language" (YAML no es un lenguaje de marcado), es un estándar de serialización de datos amigable para los humanos que puede ser utilizado para todos los lenguajes de programación. Los programadores a menudo utilizan YAML en archivos de configuración e intercambio de datos entre lenguajes debido a su legibilidad y estructura sencilla.

## Cómo hacerlo:

Haskell no tiene soporte integrado para el procesamiento de YAML, pero puedes utilizar bibliotecas de terceros como `yaml` y `aeson` para analizar y generar datos YAML. Aquí te mostramos cómo puedes empezar:

### Leyendo YAML
Primero, añade el paquete `yaml` a las dependencias de tu proyecto. Luego, puedes usar el siguiente ejemplo para analizar un simple documento YAML:

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Data.YAML
import Data.ByteString (ByteString)
import Control.Monad.IO.Class (liftIO)

-- Ejemplo de datos YAML
yamlData :: ByteString
yamlData = "
nombre: John Doe
edad: 30
"

-- Define una estructura de datos que coincida con el documento YAML
data Persona = Persona
  { nombre :: String
  , edad :: Int
  } deriving (Show)

instance FromYAML Persona where
  parseYAML = withMap "Persona" $ \m -> Persona
    <$> m .: "nombre"
    <*> m .: "edad"

main :: IO ()
main = do
  let analizado = decode1 yamlData :: Either (Pos,String) Persona
  case analizado of
    Left err -> putStrLn $ "Error al analizar YAML: " ++ show err
    Right persona -> print persona
```
La salida de muestra para el código anterior podría ser:
```
Persona {nombre = "John Doe", edad = 30}
```

### Escribiendo YAML
Para generar YAML a partir de estructuras de datos Haskell, puedes usar las funcionalidades de codificación del paquete `yaml` como se muestra a continuación:

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Data.YAML
import Data.ByteString.Lazy.Char8 (unpack)

-- Usando la estructura de datos Persona del ejemplo anterior

persona :: Persona
persona = Persona "Jane Doe" 25

main :: IO ()
main = do
  let yamlData = encode1 persona
  putStrLn $ unpack yamlData
```
La salida de este programa será una cadena formateada en YAML:
```
nombre: Jane Doe
edad: 25
```

Estos ejemplos deben servir como un punto de partida para trabajar con YAML en Haskell. Dependiendo de tus necesidades, es posible que desees explorar características y opciones más avanzadas proporcionadas por estas bibliotecas.
