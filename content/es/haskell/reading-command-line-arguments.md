---
title:                "Lectura de argumentos de línea de comandos"
html_title:           "Haskell: Lectura de argumentos de línea de comandos"
simple_title:         "Lectura de argumentos de línea de comandos"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/haskell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## ¿Por qué?

Hay muchas razones por las que alguien podría estar interesado en aprender cómo leer argumentos de línea de comando en Haskell. Por ejemplo, puede ser útil para crear programas interactivos que puedan aceptar diferentes entradas del usuario sin necesidad de recompilar el código cada vez. También es una técnica común en la programación de sistemas y aplicaciones de línea de comandos.

## Cómo

Para leer argumentos de línea de comando en Haskell, podemos usar la función `getArgs` del módulo `System.Environment` de la biblioteca estándar. Esta función devuelve una lista de cadenas que representan los argumentos pasados en la línea de comando. Tomemos un ejemplo simple:

```Haskell
import System.Environment

main = do
  args <- getArgs
  putStrLn ("Argumentos pasados: " ++ show args)
```

Si compilamos y ejecutamos este programa con `ghc` y pasamos algunos argumentos en la línea de comando, obtendremos una salida como esta:

```bash
$ ghc args.hs
$ ./args uno dos tres
Argumentos pasados: ["uno", "dos", "tres"]
```

También podemos acceder a argumentos individuales pasados ​​en la línea de comando usando el operador `!!` y un índice de lista. Por ejemplo, si queremos obtener el segundo argumento pasado, podemos hacerlo de la siguiente manera:

```Haskell
import System.Environment

main = do
  args <- getArgs
  putStrLn ("Segundo argumento: " ++ args !! 1)
```

Si ejecutamos este ejemplo con `ghc` y pasamos algunos argumentos, obtendremos una salida como esta:

```bash
$ ghc args.hs
$ ./args uno dos tres
Segundo argumento: dos
```

También podemos tomar argumentos de línea de comando a medida que el programa se está ejecutando, usando la función `getLine` del módulo `System.IO`. Esta función nos permite tomar una entrada del usuario después de que se haya iniciado el programa. Tomemos un ejemplo:

```Haskell
import System.Environment
import System.IO

main = do
  putStrLn "Ingrese un argumento:"
  arg <- getLine
  args <- getArgs
  putStrLn ("Argumento ingresado: " ++ arg)
  putStrLn ("Argumentos pasados: " ++ show args)
```

Si compilamos y ejecutamos este programa con `ghc`, obtendremos una salida como esta:

```bash
$ ghc args.hs
$ ./args uno dos tres
Ingrese un argumento:
Argumento ingresado: cuatro
Argumentos pasados: ["uno", "dos", "tres"]
```

## Exploración en profundidad

Leer argumentos de línea de comando en Haskell es una técnica simple pero poderosa que puede ser útil en una variedad de situaciones. Algunas cosas a tener en cuenta:

- Puede ser útil utilizar una lista de patrones cuando se trabajan con argumentos de línea de comando en lugar de un patrón de cadena única. Por ejemplo, en lugar de `putStrLn "Ingrese un argumento:`, podemos utilizar `putStrLn ["[Ingrese]", "[el]", "[argumento:]"]`. Esto nos permitirá manejar diferentes formas de entrada del usuario.
- Además de `getArgs`, también podemos usar la función `getProgName` del módulo `System.Environment` para obtener el nombre del programa que se está ejecutando.
- También podemos utilizar la biblioteca `Options.Applicative` para crear opciones y argumentos de línea de comando más complejos y estructurados. Esta biblioteca nos permite definir argumentos opcionales, valores de ayuda y opciones de subcomandos.

## Ver también

- [System.Environment](https://hackage.haskell.org/package/base/docs/System-Environment.html) - Documentación del módulo System.Environment en Hackage.
- [Options.Applicative](https://hackage.haskell.org/package/optparse-applicative) - Biblioteca de opciones e información de línea de comando.