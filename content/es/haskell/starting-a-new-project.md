---
title:                "Comenzando un nuevo proyecto"
html_title:           "Haskell: Comenzando un nuevo proyecto"
simple_title:         "Comenzando un nuevo proyecto"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/haskell/starting-a-new-project.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

Empezar un nuevo proyecto en programación es el proceso de iniciar un nuevo software o aplicación desde cero. Los programadores lo hacen para crear nuevas soluciones, mejorar sus habilidades y explorar nuevas tecnologías.

## ¿Cómo hacerlo?

Uno puede iniciar un nuevo proyecto en Haskell siguiendo estos pasos:

1. Crear una carpeta para el proyecto y navegar a ella en la terminal.

2. Iniciar un archivo `.hs` en blanco para escribir el código en Haskell.

3. Importar los módulos necesarios de la biblioteca estándar o cualquier biblioteca externa que se vaya a utilizar en el proyecto.

4. Escribir el código del proyecto utilizando la sintaxis de Haskell y siguiendo buenas prácticas de programación.

5. Compilar y ejecutar el código utilizando un compilador de Haskell como GHC.

Esto es un ejemplo de un programa simple que imprime "¡Hola Mundo!" en la terminal:

```Haskell
-- Importar el módulo necesario
import System.IO

-- Definir la función principal
main :: IO()
main = do
    -- Imprimir "¡Hola Mundo!" en la terminal
    putStrLn "¡Hola Mundo!"
```

Y aquí está el resultado cuando se ejecuta este programa:

```
$ ghc hola-mundo.hs
$ ./hola-mundo
¡Hola Mundo!
```

## Profundizando

Haskell es un lenguaje de programación funcional puro y altamente expresivo que fue desarrollado en 1990. Su sintaxis es más concisa y legible que otros lenguajes, lo que lo hace ideal para iniciar nuevos proyectos y aprender a programar.

Otras alternativas para iniciar un nuevo proyecto en Haskell son utilizar un entorno de desarrollo integrado (IDE) como Atom o Visual Studio Code con el complemento adecuado para programación en Haskell, o utilizar un administrador de proyectos como Stack.

En cuanto a la implementación, la compilación y ejecución del código en Haskell se realiza en dos pasos separados, lo que permite una mayor flexibilidad en el desarrollo del proyecto.

## Ver también

- [Haskell Wiki](https://wiki.haskell.org/)
- [Learn You a Haskell for Great Good](http://learnyouahaskell.com/)
- [Hackage - Haskell Package Database](https://hackage.haskell.org/)