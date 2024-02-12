---
title:                "Iniciando un nuevo proyecto"
aliases:
- /es/haskell/starting-a-new-project.md
date:                  2024-01-20T18:04:06.661037-07:00
model:                 gpt-4-1106-preview
simple_title:         "Iniciando un nuevo proyecto"

tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/haskell/starting-a-new-project.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?
Iniciar un nuevo proyecto en Haskell es abrir la puerta a un mundo de programación funcional pura. Los programadores emprenden esta tarea para explorar soluciones elegantes, resolver problemas complejos o experimentar con conceptos avanzados de tipo y función.

## Cómo Hacerlo:
Para comenzar un nuevo proyecto en Haskell, lo más común es usar Stack, una herramienta de construcción que facilita la gestión de proyectos. Aquí tienes un ejemplo rápido:

```Haskell
-- Instala Stack desde https://docs.haskellstack.org/en/stable/install_and_upgrade/
-- Abre tu terminal y ejecuta los siguientes comandos:

-- 1. Crear un nuevo proyecto de Stack:
stack new mi-proyecto

-- 2. Navega al directorio del proyecto:
cd mi-proyecto

-- 3. Construir el proyecto:
stack build

-- 4. Ejecutar el programa:
stack exec mi-proyecto-exe

-- Salida esperada (por defecto, imprime "someFunc" en Main):
someFunc
```

## Profundizando:
Haskell data de 1990, nombrado en honor al lógico Haskell Curry. Stack es relativamente nuevo en la escena (debutó en 2014), pero se ha convertido en la herramienta de elección para muchos programadores de Haskell, principalmente por su enfoque en la reproducibilidad del ambiente de trabajo y la gestión de dependencias.

Alternativas como Cabal existen desde antes y también son populares. La diferencia clave es que Stack se asegura de que todos quienes trabajan en el proyecto usen la misma configuración y versiones de las bibliotecas.

En cuanto a detalles de implementación, Stack trabaja descargando y aislado un compilador GHC (Glasgow Haskell Compiler) para tu proyecto, asegurando que no haya conflictos con otros proyectos en tu sistema. Usa un archivo 'stack.yaml' para controlar la versión de la herramienta de compilación, y un archivo 'package.yaml' o un '.cabal' para especificar las dependencias del proyecto.

## Ver También:
- Documentación oficial de Stack: [Stack Documentation](https://docs.haskellstack.org/en/stable/README/)
- Tutorial para aprender Haskell: [Learn You a Haskell for Great Good!](http://learnyouahaskell.com/)
- Haskell Homepage: [Haskell.org](https://www.haskell.org/)
- Sistema de construcción Cabal: [Cabal user guide](https://www.haskell.org/cabal/users-guide/)

Por favor, visita estos enlaces para profundizar en tu aprendizaje y práctica de Haskell. ¡Feliz codificación!
