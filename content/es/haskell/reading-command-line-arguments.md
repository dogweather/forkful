---
title:                "Haskell: Leyendo argumentos de línea de comandos"
simple_title:         "Leyendo argumentos de línea de comandos"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/haskell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## ¿Por qué necesitamos leer argumentos de línea de comandos?

Los argumentos de línea de comandos son una forma común de que los usuarios interactúen con programas de línea de comandos, como los programas escritos en Haskell. Al leer argumentos de línea de comandos, podemos personalizar el comportamiento de nuestro programa y hacerlo más versátil y útil para diferentes situaciones.

## Cómo leer argumentos de línea de comandos en Haskell

Para leer argumentos de línea de comandos en Haskell, utilizamos la función `getArgs` del módulo `System.Environment`. Esta función devuelve una lista de cadenas de texto que representan los argumentos pasados al programa de línea de comandos.

Veamos un ejemplo de un programa que lee argumentos de línea de comandos y los imprime:

```Haskell
import System.Environment

main = do
    args <- getArgs
    putStrLn ("Los argumentos pasados son: " ++ show args)
```
Al ejecutar este programa con los argumentos `hola mundo`, obtendremos la siguiente salida:

```
Los argumentos pasados son: ["hola", "mundo"]
```

Podemos acceder a cada argumento individualmente utilizando la función `!!` y el índice del argumento que deseamos. Por ejemplo, si queremos acceder al primer argumento, podemos hacerlo de la siguiente manera:

```Haskell
primerArgumento = args !! 0
```

También podemos utilizar patrones de coincidencia para trabajar con argumentos específicos. Por ejemplo, si queremos imprimir la suma de dos números pasados como argumentos, podemos hacerlo de la siguiente manera:

```Haskell
import System.Environment

main = do
    (arg1:arg2:_) <- getArgs
    let resultado = read arg1 + read arg2 :: Int
    putStrLn ("La suma de " ++ arg1 ++ " y " ++ arg2 ++ " es " ++ show resultado)
```

## Profundizando en la lectura de argumentos de línea de comandos

Al leer argumentos de línea de comandos, es importante tener en cuenta que el primer argumento siempre será el nombre del programa que se está ejecutando. También es importante tomar en cuenta el orden en el que se pasan los argumentos, ya que esto afectará cómo los accedemos en nuestro código.

En caso de que nuestros argumentos utilicen caracteres especiales, debemos tener en cuenta cómo estos serán interpretados por el sistema operativo y nuestro programa.

## Vea también

- [Documentación oficial de la función `getArgs`](https://hackage.haskell.org/package/base-4.14.1.0/docs/System-Environment.html#v:getArgs)
- [Tutorial de lectura de argumentos de línea de comandos en Haskell](https://www.codewars.com/kata/haskell/read-arguments) (en inglés)
- [Cómo pasar argumentos de línea de comandos a un programa Haskell](https://www.haskellforall.com/2020/05/passing-command-line-arguments-in-haskell.html) (en inglés)