---
date: 2024-01-26 01:02:35.800030-07:00
description: "El registro (\"logging\") es esencialmente el proceso de grabar eventos\
  \ y salidas de datos de un software mientras se ejecuta; piense en ello como el\
  \ diario\u2026"
lastmod: '2024-03-13T22:44:58.999461-06:00'
model: gpt-4-1106-preview
summary: El registro ("logging") es esencialmente el proceso de grabar eventos y salidas
  de datos de un software mientras se ejecuta; piense en ello como el diario del software.
title: "Registro de Actividades en Programaci\xF3n"
weight: 17
---

## Cómo hacerlo:
La arquitectura de Elm no soporta efectos secundarios como el registro directamente – usted los maneja a través de comandos, los cuales son una parte de la arquitectura de su aplicación. Con fines educativos, veamos cómo podría simular el registro enviando mensajes a JavaScript a través de puertos.

Primero, definirá un módulo de puerto:

```Elm
port module Logger exposing (..)

-- Definir un puerto para enviar registros a JavaScript
port log : String -> Cmd msg
```

En su `Main.elm`, utilizaría el puerto `log` para enviar un mensaje de registro:

```Elm
import Logger exposing (log)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        AnEvent ->
            -- algunas actualizaciones a su modelo aquí
            ( updatedModel, log "Ha ocurrido AnEvent." )

        AnotherEvent ->
            -- otras actualizaciones de modelo aquí
            ( anotherUpdatedModel, log "Ha ocurrido AnotherEvent." )
```

En el lado de JavaScript, se suscribiría al puerto `log` para manejar los mensajes de registro entrantes:

```JavaScript
var app = Elm.Main.init({ /* ... */ });

app.ports.log.subscribe(function(message) {
    console.log(message);
});
```

La salida de muestra en la consola de JavaScript sería entonces:

```
Ha ocurrido AnEvent.
Ha ocurrido AnotherEvent.
```

## Análisis en Profundidad
Tradicionalmente, en lenguajes como Python o Java, el registro se realiza utilizando una biblioteca de registro, la cual proporciona una API sencilla para registrar mensajes en varios niveles, como debug, info, warning, error y critical.

Elm, con su enfoque en la pureza e inmutabilidad, no proporciona este tipo de registro directo, ya que cualquier tipo de E/S o efecto secundario es gestionado de forma distinta a través de la arquitectura de Elm.

Cuando necesita un registro completo en Elm, típicamente depende de herramientas externas de JavaScript. Los puertos, como se mostró anteriormente, son el puente hacia estas herramientas. El módulo Debug es otra opción, pero está destinado solo para uso de desarrollo y no para registro de producción.

Además de los puertos, los programadores a menudo hacen uso de los mensajes del compilador de Elm y de las facilidades de depuración en tiempo de ejecución, como `Debug.log`, que puede insertar en su código para rastrear valores. Envuelve una expresión y registra su salida en la consola de la siguiente manera:

```Elm
view model =
    Debug.log "Depuración del Modelo" model
    -- su código de vista aquí
```

Esto tampoco está destinado para producción. Herramientas como elm-logger proporcionan algunas abstracciones sobre los puertos para el registro, aunque también están destinadas más para el desarrollo que para la producción.

## Vea También
- Puertos Elm: https://guide.elm-lang.org/interop/ports.html
- Elm `Debug`: https://package.elm-lang.org/packages/elm/core/latest/Debug
- Discusión en Elm sobre registro: https://discourse.elm-lang.org/t/elm-and-logging/546
- API de Consola de JavaScript: https://developer.mozilla.org/es/docs/Web/API/Console
- Paquete elm-logger: https://package.elm-lang.org/packages/arkgil/elm-logger/latest/
