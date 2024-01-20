---
title:                "Leyendo un archivo de texto"
html_title:           "Arduino: Leyendo un archivo de texto"
simple_title:         "Leyendo un archivo de texto"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elm/reading-a-text-file.md"
---

{{< edit_this_page >}}

## ¿Qué & Por qué?

Leer un archivo de texto es el proceso de obtener datos de un archivo y ponerlo a disposición para uso y manipulación por parte del programa. Los programadores hacen esto para trabajar con datos persistentes y para interactuar con otros sistemas y recursos.

## Cómo hacerlo

Actualmente, Elm no admite la lectura nativa de archivos de texto. Por lo menos, no desde el lado del cliente. Puede interactuar con APIs de JavaScript a través de "ports" para hacer esto.

```Elm
port module Main exposing (..)

type alias Model =
    { fileContent : String
    }

init : Model
init =
    { fileContent = ""
    }

port readFile : (String -> msg) -> Sub msg
```

En el lado de JavaScript, puedes usar FileReader API en el siguiente sentido:

```javascript
var app = Elm.Main.init();

app.ports.readFile.subscribe(function() {
    var input = document.createElement('input');
    input.type = 'file';

    input.addEventListener('change', function(e) {
        var file = e.target.files[0];
        var reader = new FileReader();

        reader.onload = function(e) {
            app.ports.fileContent.send(e.target.result);
        };

        reader.readAsText(file);
    });

    input.click();
});
```

## Inmersión Profunda

Históricamente, Elm tiende a mantener una postura firme sobre las interacciones directas con el sistema de archivos para mantener una fuerte garantía de seguridad para el cliente. Hoy en día, este esfuerzo continúa y se manifiesta en la incapacidad de Elm de leer directamente archivos de texto.

Como alternativa, los puertos de Elm permiten comunicaciones seguras y predecibles con JavaScript, permitiendo a los programadores interactuar con las APIs de JavaScript.

La implementación detallada no es posible en el lado de Elm debido a restricciones de seguridad y diseño, por lo que debes usar JavaScript para esta tarea.

## Ver También

- Elm Guide on Ports: [https://guide.elm-lang.org/interop/ports.html](https://guide.elm-lang.org/interop/ports.html)
- FileReader API Documentation: [https://developer.mozilla.org/en-US/docs/Web/API/FileReader](https://developer.mozilla.org/en-US/docs/Web/API/FileReader)
- Elm Language: A Deep Dive: [https://elmprogramming.com/](https://elmprogramming.com/)
- Practical Elm for a Busy Developer: [https://korban.net/posts/elm/2019-03-06-practical-elm-for-busy-developer/](https://korban.net/posts/elm/2019-03-06-practical-elm-for-busy-developer/)