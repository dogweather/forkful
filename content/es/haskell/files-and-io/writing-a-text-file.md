---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:27:52.926442-07:00
description: "C\xF3mo hacerlo: El Prelude est\xE1ndar de Haskell proporciona soporte\
  \ elemental para escribir en archivos usando las funciones `writeFile` y `appendFile`\
  \ del\u2026"
lastmod: '2024-03-13T22:44:59.136809-06:00'
model: gpt-4-0125-preview
summary: "El Prelude est\xE1ndar de Haskell proporciona soporte elemental para escribir\
  \ en archivos usando las funciones `writeFile` y `appendFile` del m\xF3dulo `System.IO`."
title: Escribiendo un archivo de texto
weight: 24
---

## Cómo hacerlo:
El Prelude estándar de Haskell proporciona soporte elemental para escribir en archivos usando las funciones `writeFile` y `appendFile` del módulo `System.IO`. Aquí hay un ejemplo básico de cómo crear un archivo nuevo (o sobrescribir uno existente) y luego agregar texto a un archivo.

```haskell
import System.IO

-- Escribiendo en un archivo, sobrescribiéndolo si existe
main :: IO ()
main = do
  writeFile "example.txt" "Esta es la línea uno.\n"
  appendFile "example.txt" "Esta es la línea dos.\n"
```

Cuando ejecutas este programa, crea (o limpia) `example.txt` y escribe "Esta es la línea uno." seguido de "Esta es la línea dos." en la siguiente línea.

Para un manejo de archivos más avanzado, los programadores de Haskell a menudo recurren al paquete `text` para un procesamiento eficiente de cadenas y al paquete `bytestring` para manejar datos binarios. Aquí tienes cómo usar el paquete `text` para IO de archivos:

Primero, necesitas agregar `text` a las dependencias de tu proyecto. Luego, puedes usarlo de la siguiente manera:

```haskell
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

-- Escribiendo en un archivo usando el paquete text
main :: IO ()
main = do
  let content = T.pack "Usando el paquete text para un mejor rendimiento.\n"
  TIO.writeFile "textExample.txt" content
  TIO.appendFile "textExample.txt" $ T.pack "Agregando línea dos.\n"
```

En este fragmento, `T.pack` convierte una `String` regular al tipo `Text`, que es más eficiente. `TIO.writeFile` y `TIO.appendFile` son los equivalentes en `text` para escribir y agregar a archivos, respectivamente.

Ejecutar este código resultará en un archivo llamado `textExample.txt` con dos líneas de texto, demostrando tanto las capacidades de creación como de agregación usando la avanzada biblioteca `text` para un mejor rendimiento y capacidad en el manejo de texto Unicode.
