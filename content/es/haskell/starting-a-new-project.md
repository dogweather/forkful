---
title:                "Iniciando un nuevo proyecto"
html_title:           "Bash: Iniciando un nuevo proyecto"
simple_title:         "Iniciando un nuevo proyecto"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/haskell/starting-a-new-project.md"
---

{{< edit_this_page >}}

**## ¿Qué & Por qué?:**
Iniciar un nuevo proyecto en programación es crear desde cero una aplicación o software nuevo, con su propia lógica y estructura. Los programadores lo hacen para resolver problemas específicos o innovar con nuevas funcionalidades y sistemas.

**## Cómo hacerlo:**

Aquí tienes un ejemplo sencillo para empezar un proyecto Haskell con Stack. Se crea un proyecto simple llamado "hola".

```Haskell
-- Primero, en la línea de comando, ejecuta:
stack new hola simple

-- Esto genera un proyecto con la estructura de carpetas y archivos
-- Aquí tienes la función principal en el archivo "Main.hs"
main :: IO ()
main = putStrLn "¡Hola, mundo!"
```
Para ejecutar este programa, ve a la carpeta 'hola' y ejecuta `stack run`.

**## Más Información:**

Para profundizar un poco más, el comando `stack new` es parte de la utilidad de Haskell llamada Stack. Esta herramienta fue creada para abordar los problemas de construcción y gestión de proyectos en Haskell. Antes de Stack, Cabal era la herramienta de facto, pero cada una de ellas tiene sus propias ventajas.

Una alternativa a usar 'simple' como plantilla de Stack podría ser 'new-template'. Esta hace un proyecto con más estructura y varios módulos, lo cual podría ser útil para proyectos más grandes.

**## Ver También:**

Para continuar aprendiendo, te recomendamos los siguientes recursos:

- ["Learn You a Haskell for Great Good!"](http://learnyouahaskell.com/)
- [Sitio Web Oficial de Haskell](https://www.haskell.org/)
- [Documentación de Stack](https://docs.haskellstack.org/en/stable/README/) (inglés)