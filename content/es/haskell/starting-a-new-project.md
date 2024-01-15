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

## ¿Por qué?

Iniciar un nuevo proyecto en Haskell puede ser una gran oportunidad para aprender un lenguaje de programación funcional y expandir tus habilidades como desarrollador. Además, Haskell se caracteriza por tener un sistema de tipos robusto y herramientas como el *ghci* que te permiten probar y depurar tu código de forma interactiva.

## Cómo hacerlo

Iniciar un nuevo proyecto en Haskell es fácil y sencillo. Solo debes seguir estos pasos:

1. Instalar [Haskell Platform](https://www.haskell.org/platform/)
2. Crear un nuevo directorio para tu proyecto
3. Abrir tu terminal y navegar al directorio recién creado
4. Iniciar el *ghci* ejecutando el comando `ghci` en tu terminal
5. Dentro del *ghci*, importa el módulo `Cabal` ejecutando `import Distribution.Simple` 
6. Ahora puedes empezar a escribir tu código en un archivo con extensión `.hs` y guardarlo en el directorio de tu proyecto
7. En tu terminal, ejecuta el comando `runhaskell nombreDeTuArchivo.hs` para compilar y ejecutar tu código
8. ¡Listo! Ya tienes tu primer proyecto en Haskell.

Aquí te dejamos un ejemplo de código para que puedas probarlo por ti mismo:

```Haskell
main :: IO ()
main = do
  putStrLn "¡Hola mundo!"
```
Este sencillo programa imprime "¡Hola mundo!" en la consola cuando se ejecuta. Ahora es tu turno de experimentar y seguir aprendiendo sobre Haskell.

## Profundizando

Si quieres profundizar en el proceso de iniciar un nuevo proyecto en Haskell, te recomendamos leer más sobre el formato de archivo `cabal` y cómo escribir un archivo `Setup.hs` para personalizar tu proceso de compilación. Además, puedes investigar sobre el uso de herramientas como *Stack* para manejar dependencias en tus proyectos de Haskell.

## Ver también

- [Learn You a Haskell](http://learnyouahaskell.com/) - un gran recurso para aprender Haskell desde cero.
- [Haskell.org](https://www.haskell.org/) - sitio oficial de Haskell con información, documentación y enlaces útiles.
- [Real World Haskell](http://book.realworldhaskell.org/) - un libro en línea gratuito sobre Haskell y su uso en proyectos reales.