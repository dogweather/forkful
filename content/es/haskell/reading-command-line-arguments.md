---
title:                "Leyendo argumentos de la línea de comandos"
html_title:           "Bash: Leyendo argumentos de la línea de comandos"
simple_title:         "Leyendo argumentos de la línea de comandos"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/haskell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# Leyendo Argumentos de la Línea de Comandos en Haskell

## ¿Qué y Por Qué?

La lectura de argumentos de la línea de comandos es cómo tu programa recoge información de usuarios al momento de ejecutarlo. Los programadores hacen esto para añadir flexibilidad a la hora de manejar varios casos de uso de un programa.

## Cómo hacerlo:

Empezaremos importando el módulo `System.Environment` de Haskell para poder trabajar con los argumentos de la línea de comandos. 

```Haskell
import System.Environment 
```

Para leer los argumentos de línea de comandos, usamos la función `getArgs`:

```Haskell
main = do
    args <- getArgs
    print args
```

Cada vez que ejecutas este programa desde la línea de comandos, imprimirá todos los argumentos que tu pongas después del nombre del programa. Por ejemplo, `runghc main.hs primerArg segundoArg` imprimirá `["primerArg","segundoArg"]`.

## Profundizando

Primero, un poco de contexto histórico: Los argumentos de la línea de comandos han sido usados desde los primeros días del desarrollo del software, y proporcionan una forma poderosa y flexible de controlar el comportamiento del programa.

Hay alternativas a pasar argumentos a través de la línea de comandos, tales como leer un archivo de configuración o solicitar información al usuario a través de una interfaz gráfica de usuario. Sin embargo, los argumentos de la línea de comandos siguen siendo esenciales, especialmente para la automatización y la depuración.

En cuanto a la implementación, `getArgs` proviene del módulo `System.Environment` de Haskell, que proporciona funciones para interactuar con el entorno de ejecución del programa. `getArgs` simplemente lee el listado de argumentos que el sistema operativo pasa al programa.

## Consultar también:

1. Documentación de Haskell sobre getArgs en Hackage [(enlace)](http://hackage.haskell.org/package/base-4.9.1.0/docs/System-Environment.html#v:getArgs)
2. Tutorial de Haskell sobre argumentos de la línea de comandos [(enlace)](https://wiki.haskell.org/Command_line_option_parsers)