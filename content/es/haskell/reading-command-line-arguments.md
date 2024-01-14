---
title:                "Haskell: Leyendo los argumentos de línea de comando"
programming_language: "Haskell"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/haskell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# Por qué leer argumentos de línea de comando en Haskell

Leer argumentos de línea de comando en Haskell puede ser muy útil para crear programas más dinámicos y versátiles. Con esta habilidad, puedes tomar entrada directamente del usuario en la terminal y usarla en tu código de Haskell.

## Cómo hacerlo

Para leer los argumentos de línea de comando en Haskell, primero necesitamos importar el módulo System.Environment. Luego, podemos usar la función `getArgs` para obtener una lista de todos los argumentos ingresados por el usuario.

```
import System.Environment

main = do
args <- getArgs
print args
```

Al ejecutar este código y pasar argumentos en la terminal, obtendremos una lista de esos argumentos como salida. Por ejemplo, si ejecutamos `ghc programa.hs` y luego `./programa argumento1 argumento2`, la salida será `[argumento1, argumento2]`.

## Profundizando

Ahora que sabemos cómo leer los argumentos de línea de comando, podemos utilizarlos en nuestro código de Haskell de diversas maneras. Podemos acceder a cada argumento individualmente usando las funciones `!!` y `head` para obtener el primer elemento de la lista de argumentos. También podemos convertir los argumentos en tipos de datos diferentes usando funciones como `read` o `fromString`.

Además, podemos combinar la lectura de argumentos de línea de comando con otras funciones de entrada de usuario, como `getLine` para crear un programa altamente interactivo.

# Ver también

- Documentación del módulo System.Environment: https://hackage.haskell.org/package/base-4.14.1.0/docs/System-Environment.html
- Tutorial de Haskell para principiantes: https://www.haskell.org/tutorial/
- Ejemplos de código de Haskell: https://github.com/Haskell/Exercises