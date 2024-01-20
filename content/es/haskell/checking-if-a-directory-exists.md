---
title:                "Verificando si un directorio existe"
html_title:           "Haskell: Verificando si un directorio existe"
simple_title:         "Verificando si un directorio existe"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/haskell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## ¿Qué y Por qué?

Verificar si existe un directorio es una tarea básica en cualquier lenguaje de programación. Un directorio es un archivo del sistema que contiene otros archivos y directorios. Como programador, podrías necesitar verificar si un directorio existe antes de intentar leer, escribir o manipular de alguna manera los contenidos del directorio.

## Cómo hacerlo:

En Haskell, usamos el módulo `System.Directory` y la función `doesDirectoryExist` como se muestra:

```Haskell
import System.Directory
main = do
  let path = "/path/to/directory"
  dirExists <- doesDirectoryExist path
  print dirExists
```

Ejecutar este código devolverá `True` si el directorio especificado existe, y `False` si no existe.

## Profundización:

Verificar si un directorio existe no siempre ha sido tan directo en los lenguajes de programación. En los primeros días de los sistemas operativos, los programas tenían que interactuar directamente con el sistema operativo para realizar estas comprobaciones.

Usar `System.Directory` y `doesDirectoryExist` no es la única forma de hacerlo en Haskell. Por ejemplo, puedes usar la biblioteca `Shelly` que es más de alto nivel:

```Haskell
import Shelly
main = do
  let path = "/path/to/directory"
  dirExists <- shelly $ test_d path
  print dirExists
```

La función `doesDirectoryExist` utiliza la interacción del sistema operativo a través de FFI (Interfaz de Funciones Extranjeras).

## Consulta también:

- Módulo `System.Directory`: http://hackage.haskell.org/package/directory-1.3.6.1/docs/System-Directory.html
- Módulo `Shelly`: http://hackage.haskell.org/package/shelly-1.9.0/docs/Shelly.html

Evita las complicaciones y verifica siempre si un directorio existe antes de intentar operar con él en tu código.