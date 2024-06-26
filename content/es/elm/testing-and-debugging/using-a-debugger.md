---
date: 2024-01-26 03:48:47.155825-07:00
description: "C\xF3mo hacerlo: Elm no tiene un depurador integrado en el sentido tradicional\
  \ que, digamos, JavaScript tiene con las herramientas de desarrollo del\u2026"
lastmod: '2024-03-13T22:44:58.997386-06:00'
model: gpt-4-0125-preview
summary: Elm no tiene un depurador integrado en el sentido tradicional que, digamos,
  JavaScript tiene con las herramientas de desarrollo del navegador.
title: Usando un depurador
weight: 35
---

## Cómo hacerlo:
Elm no tiene un depurador integrado en el sentido tradicional que, digamos, JavaScript tiene con las herramientas de desarrollo del navegador. Sin embargo, la comunidad de Elm ha creado herramientas para llenar este vacío. Así es cómo puedes usar `elm-debug-transformer` para depurar tu aplicación Elm:

```Elm
-- Instalar elm-debug-transformer (paquete Node)

1. npm install -g elm-debug-transformer

-- Usar elm-debug-transformer para iniciar tu aplicación

2. elm-debug-transformer --port=8000 yourMainElmFile.elm 
```

Una vez que `elm-debug-transformer` está en funcionamiento, crea una conexión WebSocket para registrar. Verás la información de depuración en la consola de tu navegador donde puedes inspeccionar las estructuras de datos de tu programa en puntos dados de tu aplicación.

En Elm 0.19 y posteriores, las funciones del módulo `Debug` como `Debug.log` y `Debug.todo` pueden ayudarte a rastrear valores y marcar deliberadamente partes inacabadas de tu código. Así es cómo usar Debug.log:

```Elm
import Debug

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Increment ->
            ( Debug.log "Incrementando" { model | count = model.count + 1 }, Cmd.none )

        Decrement ->
            ( Debug.log "Decrementando" { model | count = model.count - 1 }, Cmd.none )
```

Verás mensajes de "Incrementando" o "Decrementando" en la consola de tu navegador junto con el nuevo estado del `modelo`.

## Profundizando
El autor de Elm, Evan Czaplicki, se propuso hacer un lenguaje donde los errores comunes fueran imposibles de cometer o fáciles de detectar. Esta filosofía es la razón por la cual el núcleo de Elm no incluye funciones de depuración tradicionales. El análisis estático de Elm y la inferencia de tipos contribuyen masivamente a reducir los errores en tiempo de ejecución, lo que disminuye la necesidad de depuración sofisticada en tiempo de ejecución. Alternativas históricas incluían el uso del ahora obsoleto `elm-reactor` que ofrecía depuración de viaje en el tiempo: una forma de rebobinar y repetir acciones en tu aplicación.

Hoy en día, herramientas como `elm-debug-transformer` y el uso del módulo `Debug` de Elm ayudan a cerrar esta brecha. Aunque el módulo `Debug` está destinado para uso durante el desarrollo únicamente y debe ser eliminado antes de las construcciones de producción, es una herramienta invaluable para identificar y registrar cambios de estado.

Ten en cuenta que las técnicas de depuración de JavaScript tradicionales, como puntos de interrupción o ejecución paso a paso, no son directamente aplicables en Elm debido a su arquitectura y la forma en que el tiempo de ejecución de Elm maneja las actualizaciones de estado. Elm te anima a estructurar tu programa de tal manera que el flujo de datos sea claro y siga garantías estrictas de tipos e inmutabilidad, minimizando los casos en los que se necesita depuración.

## Ver También
- La guía oficial de Elm sobre el manejo de excepciones en tiempo de ejecución: https://guide.elm-lang.org/error_handling/
- Repositorio de GitHub de `elm-debug-transformer`: https://github.com/kraklin/elm-debug-transformer
- Hilo de discusión de Elm sobre estrategias de depuración: https://discourse.elm-lang.org/c/show-and-tell/debugging
- Documentación del módulo `Debug` de Elm: https://package.elm-lang.org/packages/elm/core/latest/Debug
