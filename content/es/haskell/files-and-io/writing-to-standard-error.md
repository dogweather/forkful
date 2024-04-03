---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:33:16.499038-07:00
description: "C\xF3mo hacerlo: En Haskell, escribir en stderr es sencillo con el m\xF3\
  dulo `System.IO` de la librer\xEDa base. A continuaci\xF3n se muestra un ejemplo\
  \ b\xE1sico para\u2026"
lastmod: '2024-03-13T22:44:59.134709-06:00'
model: gpt-4-0125-preview
summary: "En Haskell, escribir en stderr es sencillo con el m\xF3dulo `System.IO`\
  \ de la librer\xEDa base."
title: "Escribiendo en el error est\xE1ndar"
weight: 25
---

## Cómo hacerlo:
En Haskell, escribir en stderr es sencillo con el módulo `System.IO` de la librería base. A continuación se muestra un ejemplo básico para demostrarlo:

```haskell
import System.IO

main :: IO ()
main = do
  hPutStrLn stderr "Este es un mensaje de error."
```

La salida de este programa en stderr sería:

```
Este es un mensaje de error.
```

Si estás trabajando en una aplicación más compleja, o si necesitas un mejor control sobre el registro de eventos (incluidos los errores), podrías optar por una librería de terceros. Una elección popular es `monad-logger` que se integra con el estilo de programación `mtl` de Haskell. Aquí hay un pequeño fragmento utilizando `monad-logger`:

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Control.Monad.Logger

main :: IO ()
main = runStderrLoggingT $ do
  logErrorN "Este es un mensaje de error usando monad-logger."
```

Cuando se ejecuta, la versión de `monad-logger` igualmente produce un mensaje de error, pero está equipada con más contexto, como marcas de tiempo o niveles de registro, dependiendo de la configuración:

```
[Error] Este es un mensaje de error usando monad-logger.
```

Ambos métodos sirven para el propósito de escribir en stderr, con la elección dependiendo en gran medida de la complejidad y las necesidades de tu aplicación.
