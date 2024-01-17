---
title:                "Trabajando con yaml"
html_title:           "Haskell: Trabajando con yaml"
simple_title:         "Trabajando con yaml"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/haskell/working-with-yaml.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

Trabajar con YAML es una forma común para que los programadores manejen la configuración de sus aplicaciones. Está basado en la simple idea de usar etiquetas y valores para definir datos estructurados, similar a cómo lo hacemos al escribir JSON.

Los programadores utilizan YAML porque les permite definir información de manera clara y organizada, lo que facilita la lectura y escritura de código. 

## Cómo hacerlo:

Utilizar YAML en Haskell es muy sencillo. Primero, debemos importar el módulo correspondiente:

```Haskell
import Data.Yaml
```

Luego, podemos cargar un archivo YAML utilizando la función `decodeFile` y especificando el tipo de datos que esperamos obtener:

```Haskell
config <- decodeFile "config.yaml" :: IO (Maybe Config)
```

Finalmente, podemos acceder a los datos del archivo YAML usando la función `(.:)`:

```Haskell
host <- config .: "host"
port <- config .: "port"
```

## Profundizando:

YAML fue creado originalmente en 2001 por Clark Evans como una alternativa más legible al formato XML. Desde entonces, se ha convertido en un estándar para la configuración de aplicaciones y sistemas.

Aunque YAML tiene muchas ventajas, también existen otras opciones como JSON o XML. Cada uno tiene sus propias ventajas y desventajas, por lo que es importante elegir la adecuada según tus necesidades.

La implementación de YAML en Haskell se basa en el módulo `Data.Yaml`, que utiliza algoritmos de parsing de alta eficiencia y una sintaxis simple para leer y escribir archivos YAML.

## Ver también:

- Documentación oficial de YAML en Haskell: https://www.stackage.org/haddock/lts-18.4/yaml-0.11.4.0/Data-Yaml.html
- Tutorial de utilización de YAML en Haskell: http://travis-ci.org/p/yaml-hs