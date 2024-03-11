---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:19:48.921006-07:00
description: "Trabajar con CSVs (Valores Separados por Comas) implica analizar y generar\
  \ archivos que almacenan datos tabulares en un formato de texto simple. Los\u2026"
lastmod: '2024-03-11T00:14:32.956865-06:00'
model: gpt-4-0125-preview
summary: "Trabajar con CSVs (Valores Separados por Comas) implica analizar y generar\
  \ archivos que almacenan datos tabulares en un formato de texto simple. Los\u2026"
title: Trabajando con CSV
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?

Trabajar con CSVs (Valores Separados por Comas) implica analizar y generar archivos que almacenan datos tabulares en un formato de texto simple. Los programadores frecuentemente realizan esta tarea para importar o exportar datos de manera eficiente desde hojas de cálculo, bases de datos, o para facilitar el intercambio de datos entre diferentes programas.

## Cómo hacerlo:

En Haskell, el manejo de archivos CSV se puede lograr utilizando la biblioteca `cassava`, una de las bibliotecas de terceros más populares para este propósito. A continuación, se muestran ejemplos de cómo leer y escribir en archivos CSV usando `cassava`.

**1. Leyendo un archivo CSV:**

Primero, asegúrate de tener `cassava` instalado añadiéndolo al archivo cabal de tu proyecto o usando Stack.

Aquí tienes un ejemplo simple para leer un archivo CSV e imprimir cada registro. Suponemos que el archivo CSV tiene dos columnas: nombre y edad.

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Data.Csv
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V

main :: IO ()
main = do
    csvData <- BL.readFile "people.csv"
    case decode NoHeader csvData of
        Left err -> putStrLn err
        Right v -> V.forM_ v $ \(name, age) ->
            putStrLn $ name ++ " tiene " ++ show (age :: Int) ++ " años de edad."
```

Suponiendo que `people.csv` contiene:
```
John,30
Jane,25
```
La salida será:
```
John tiene 30 años de edad.
Jane tiene 25 años de edad.
```

**2. Escribiendo un archivo CSV:**

Para crear un archivo CSV, puedes usar la función `encode` de `cassava`.

Así es como podrías escribir una lista de registros en un archivo CSV:

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Data.Csv
import qualified Data.ByteString.Lazy as BL

main :: IO ()
main = BL.writeFile "output.csv" $ encode [("John", 30), ("Jane", 25)]
```

Después de ejecutar este programa, `output.csv` contendrá:

```
John,30
Jane,25
```

Esta introducción concisa al trabajo con archivos CSV en Haskell utilizando la biblioteca `cassava` demuestra cómo leer y escribir en archivos CSV, haciendo las tareas de manipulación de datos más accesibles para aquellos nuevos en el lenguaje.
