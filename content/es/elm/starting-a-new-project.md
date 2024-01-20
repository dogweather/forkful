---
title:                "Iniciando un nuevo proyecto"
html_title:           "Bash: Iniciando un nuevo proyecto"
simple_title:         "Iniciando un nuevo proyecto"
programming_language: "Elm"
category:             "Elm"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elm/starting-a-new-project.md"
---

{{< edit_this_page >}}

## ¿Qué es y Por qué?

Iniciar un nuevo proyecto es simplemente construir una nueva aplicación o programa desde cero. Los programadores lo hacen por varias razones: para resolver problemas únicos, para aprender nuevas habilidades o lenguajes, o para crear un producto o servicio.

## ¿Cómo se hace?: 

Para empezar con Elm, necesitas instalarlo. Puedes hacerlo con npm:

``` Elm 
npm install -g elm
```

Luego crea tu primer proyecto:

``` Elm 
elm init
```

Este comando creará una nueva carpeta con todo lo necesario para iniciar un proyecto de Elm, incluso un archivo 'elm.json'.

Aquí tienes un sencillo programa en Elm:

``` Elm
import Html exposing (div, text, beginnerProgram)

main =
  beginnerProgram { model = "Hola Mundo!", view = Html.text, update = \_ _ -> "Hola Mundo!" }

```

Para ejecutarlo, escribes:

``` Elm
elm reactor
```

Ahora, abre localhost:8000 en tu navegador. Verás "Hola Mundo!".

## Buceo Profundo

El lenguaje Elm es relativamente joven y fue creado por Evan Czaplicki en 2012 como una alternativa más segura y fácil de usar a JavaScript. Su comprensión más profunda requiere estudiar su arquitectura y sintaxis, principalmente la arquitectura Model-View-Update (MVU).

Se podría utilizar JavaScript o TypeScript en lugar de Elm, pero Elm ofrece ventajas tales como una garantía de ausencia de errores en tiempo de ejecución.

Cuando inicias un nuevo proyecto Elm, en realidad se está creando un entorno de programación funcional que compila a JavaScript.

## Ver Además

1. [Elm Guide](https://guide.elm-lang.org) - Guía oficial de Elm.
2. [Elm Tutorial](https://elmprogramming.com) - Un tutorial completo para Elm.
3. [Try Elm](http://elm-lang.org/examples) - Un lugar para escribir y probar código Elm.

Nota: Todos los enlaces están en inglés.