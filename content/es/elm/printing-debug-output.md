---
title:                "Imprimiendo salida de depuración"
html_title:           "Elm: Imprimiendo salida de depuración"
simple_title:         "Imprimiendo salida de depuración"
programming_language: "Elm"
category:             "Elm"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elm/printing-debug-output.md"
---

{{< edit_this_page >}}

## Por qué

Si eres nuevo en Elm, es posible que hayas notado que a medida que escribes código, no ves el típico resultado de "imprimir en consola" que se ve en otros lenguajes. Es por eso que en este artículo te mostraré cómo imprimir la salida de depuración en Elm y por qué es una práctica útil.

## Cómo hacerlo

En Elm, tienes acceso a una función llamada `Debug.log`. Esta función toma dos argumentos: una etiqueta y un valor. La etiqueta es simplemente una cadena de texto que te permite identificar fácilmente qué se está imprimiendo, y el valor puede ser cualquier cosa, desde una cadena hasta un entero o incluso una lista.

```Elm
import Debug exposing (log)

age : Int
age = 25

log "Edad" age
```

Este código imprimirá la etiqueta "Edad" junto con el valor `25` en la consola del navegador. También puedes usar valores variables o expresiones en la etiqueta, lo que puede ser útil para distinguir entre diferentes puntos de la aplicación donde se llama a la función `Debug.log`.

```Elm
name : String
name = "John"
age : Int
age = 25

log ("Nombre: " ++ name) age
```

En este ejemplo, la etiqueta se crea mediante la combinación de la cadena "Nombre: " con el valor de la variable `name` utilizando el operador de concatenación `++`.

## Profundizando

La función `Debug.log` puede resultar muy útil para imprimir valores en la consola y verificar que tu código esté funcionando correctamente. Sin embargo, ten en cuenta que esta función solo debe usarse con fines de depuración y no debe incluirse en la versión final de tu aplicación.

Además, es importante tener en cuenta que la función `Debug.log` solo se compilará en tu código si se utiliza en un contexto de depuración, es decir, si la aplicación se está ejecutando en el modo de depuración. Por lo tanto, no afectará el rendimiento de tu aplicación en producción.

## Ver también

Para obtener más información sobre cómo depurar tu código en Elm, te recomiendo consultar la documentación oficial de Elm sobre depuración: https://guide.elm-lang.org/debugging/. También puedes aprender más sobre las mejores prácticas para imprimir valores de depuración en la consola en este artículo: https://elmprogramming.com/print-debug-values-in-elm.html.