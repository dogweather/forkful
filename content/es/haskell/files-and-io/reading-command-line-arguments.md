---
date: 2024-01-20 17:56:10.233077-07:00
description: "C\xF3mo: ."
lastmod: '2024-03-13T22:44:59.133713-06:00'
model: gpt-4-1106-preview
summary: .
title: "Lectura de argumentos de l\xEDnea de comandos"
weight: 23
---

## Cómo:
```Haskell
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  print args
```

Si guardas esto como `args.hs` y lo ejecutas:

```
$ runghc args.hs uno dos tres
["uno","dos","tres"]
```

El programa imprimirá una lista de argumentos de la línea de comandos.

## Inmersión Profunda
Históricamente, leer los argumentos de la línea de comandos es una práctica común en programas de consola y scripts. Haskell, al ser un lenguaje funcional, maneja estos argumentos en forma de lista, lo cual concuerda con su filosofía de inmutabilidad y manejo de listas.

Además de `getArgs`, tenemos `getProgName` para obtener el nombre del propio programa, y para casos más complejos, bibliotecas como `System.Console.GetOpt` para parsear opciones de línea de comandos estructuradas.

En cuanto a los detalles de implementación, `getArgs` hace una llamada al sistema operativo para obtener los argumentos pasados al ejecutable. Es importante manejar posibles errores, como argumentos mal formados o la espera de un tipo de dato que no se ha recibido.

## Ver También
Aquí hay algunos enlaces que puede ser útiles para aprender más:

- Documentación oficial de System.Environment: https://hackage.haskell.org/package/base-4.16.1.0/docs/System-Environment.html
- Tutorial sobre GetOpt en Haskell: https://www.schoolofhaskell.com/school/starting-with-haskell/libraries-and-frameworks/text-manipulation/attoparsec
- Buenos patrones para manejo de argumentos de línea de comandos en Haskell: https://wiki.haskell.org/Command_line_option_parsers
