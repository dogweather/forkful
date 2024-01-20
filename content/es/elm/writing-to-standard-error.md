---
title:                "Escribiendo en el error estándar"
html_title:           "Arduino: Escribiendo en el error estándar"
simple_title:         "Escribiendo en el error estándar"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elm/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Qué & Por qué?
Escribir en el error estándar significa mostrar mensajes aparte del flujo normal de datos. Los programadores lo hacen para reportar errores sin interferir con la salida regular del programa.

## Cómo hacerlo:
Elm corre en el navegador y no tiene acceso directo a STDERR como un lenguaje de servidor o de scripting, así que este código es conceptual y puedes ejecutar similar en JavaScript incorporado en Elm.

```Elm
port module Main exposing (..)

-- Define un puerto para reportar errores
port reportError : String -> Cmd msg

-- Manda un error usando el puerto
reportError "Este es un error estándar de ejemplo."
```

La implementación en JavaScript podría ser:

```JavaScript
app.ports.reportError.subscribe(function(errorMessage) {
  console.error(errorMessage);
});
```

Salida de ejemplo en la consola del navegador:

```plaintext
Este es un error estándar de ejemplo.
```

## Análisis Profundo
Históricamente, los programas de línea de comandos usan STDERR para permitir que se separen los errores de la salida estándar. En Elm, como en otros lenguajes que se ejecutan en el navegador como JavaScript, no hay un concepto directo de STDERR pero se puede simular con `console.error` para propósitos de depuración o para informar de errores. La idea detrás es comunicar fallas sin afectar el flujo principal del programa.

## Ver También
- Documentación de Elm sobre puertos: https://guide.elm-lang.org/interop/ports.html
- Console API en MDN para `console.error`: https://developer.mozilla.org/en-US/docs/Web/API/Console/error
- Guía de Elm: https://guide.elm-lang.org/