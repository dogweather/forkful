---
title:                "Haskell: Comenzando un nuevo proyecto"
programming_language: "Haskell"
category:             "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/haskell/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Por qué
Empezar un nuevo proyecto en Haskell puede parecer abrumador al principio, pero en realidad ofrece muchos beneficios. La programación funcional en Haskell es altamente modular y permite una fácil refactorización del código, lo que ahorra tiempo y aumenta la calidad del software.

## Cómo hacerlo
Empezar un nuevo proyecto en Haskell es sencillo. Primero, asegúrate de tener Haskell instalado en tu computadora. Luego, puedes crear un nuevo proyecto utilizando alguna herramienta como Stack o Cabal. Aquí hay un ejemplo utilizando Stack:

```Haskell
stack new mi_proyecto simple
```

Esto creará una estructura básica para tu proyecto con un archivo de cabecera, un archivo de código fuente y un archivo de prueba. También puedes comenzar desde cero creando tus propios archivos y directorios.

Una vez que tengas tu estructura de proyecto creada, es hora de empezar a escribir tu código. Aquí hay un ejemplo de una función que suma dos números en Haskell:

```Haskell
suma :: Int -> Int -> Int
suma x y = x + y
```

Esta función toma dos números enteros como entrada y devuelve su suma como resultado. Puedes probarlo en la ventana de GHCi escribiendo:

```Haskell
> suma 3 5
8
```

Esto debería imprimir el resultado en la consola. ¡Ya estás en camino de empezar tu nuevo proyecto en Haskell!

## Profundizando
Para profundizar en cómo empezar un nuevo proyecto en Haskell, es importante entender algunos de los conceptos fundamentales de la programación funcional. Esto incluye el uso de funciones puras, tipos fuertes y estáticos, y el uso de estructuras de datos inmutables. A medida que avances en tu proyecto, podrás explorar más a fondo estos conceptos y utilizarlos para escribir un código más conciso y robusto.

Además, hay una gran comunidad de programadores de Haskell a los que puedes unirte para obtener ayuda y obtener más información sobre cómo empezar proyectos en Haskell. También puedes consultar la documentación oficial y numerosos recursos en línea para seguir aprendiendo y mejorando en tu proyecto.

## Ver también
- [Documentación oficial de Haskell] (https://www.haskell.org/documentation/)
- [Documentación de Stack] (https://docs.haskellstack.org/en/stable/README/)
- [Documentación de Cabal] (https://www.haskell.org/cabal/)
- [Comunidad de programadores de Haskell] (https://www.reddit.com/r/haskell/)

Con estos recursos, deberías estar bien preparado para empezar tu nuevo proyecto en Haskell. ¡Buena suerte y feliz programación!