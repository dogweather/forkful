---
title:    "Haskell: Leyendo argumentos de línea de comando"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## Por qué

Leer argumentos de línea de comandos es una habilidad esencial para cualquier programador en Haskell. Esto te permitirá crear programas más flexibles y personalizables, y te ahorrará tiempo en la ejecución de tu código.

## Cómo hacerlo

Para leer argumentos de línea de comandos en Haskell, necesitarás utilizar la función `getArgs` del módulo `System.Environment`. Esta función toma como entrada una lista de cadenas de texto que representan los argumentos introducidos en la línea de comandos al ejecutar nuestro programa.

Veamos un ejemplo sencillo de cómo utilizar `getArgs` para imprimir los argumentos introducidos:

```Haskell
import System.Environment

main = do
    args <- getArgs
    putStrLn ("Los argumentos introducidos fueron: " ++ (show args))
```

Si ejecutas este programa desde la línea de comandos con los argumentos `Hola Mundo`, la salida será:

```
Los argumentos introducidos fueron: ["Hola","Mundo"]
```

## Profundizando

Ahora que sabemos cómo utilizar `getArgs`, podemos explorar algunas funciones adicionales del módulo `System.Environment`. Por ejemplo, la función `getProgName` nos permite obtener el nombre del programa que se está ejecutando. Podemos utilizar esta función para imprimir un mensaje personalizado basado en el nombre del programa:

```Haskell
import System.Environment

main = do
    progName <- getProgName
    putStrLn ("Bienvenido al programa " ++ progName)
```

Si ejecutas este programa desde la línea de comandos con el nombre `miPrograma`, la salida será:

```
Bienvenido al programa miPrograma
```

También podemos utilizar la función `lookupEnv` para obtener el valor de una variable de entorno específica. Por ejemplo, si queremos obtener el valor de la variable `HOME`, podemos hacer lo siguiente:

```Haskell
import System.Environment

main = do
    home <- lookupEnv "HOME"
    case home of
        Just val -> putStrLn ("El valor de HOME es " ++ val)
        Nothing -> putStrLn "La variable HOME no está definida"
```

Si la variable `HOME` está definida en tu sistema, imprimirá su valor. De lo contrario, imprimirá un mensaje informándonos de que la variable no está definida.

## Ver también

- [Documentación del módulo System.Environment](https://hackage.haskell.org/package/base-4.15.0.0/docs/System-Environment.html)
- [Tutorial de Haskell en Español](https://www.haskell-es.com/)