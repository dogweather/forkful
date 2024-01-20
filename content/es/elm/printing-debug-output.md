---
title:                "Imprimiendo salida de depuración"
html_title:           "Arduino: Imprimiendo salida de depuración"
simple_title:         "Imprimiendo salida de depuración"
programming_language: "Elm"
category:             "Elm"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elm/printing-debug-output.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?

Imprimir el output de depuración es una forma de rastrear y entender cómo se comporta el código a lo largo de su ejecución. Nos ayuda a detectar y corregir los errores del programa.

## Cómo hacerlo:

Para imprimir datos de depuración en Elm, utilizamos la función `Debug.log`. Veamos un ejemplo:

```Elm
printValue : String -> Int -> Int
printValue label value =
  Debug.log label value
```
En este código, `label` es una etiqueta descriptiva para la salida y `value` es el valor que queremos imprimir. La ejecución mostrará algo como esto:

```Elm
"myLabel" : 15
```

## Más Detalles:

1. **Contexto Histórico**: El lenguaje de programación Elm, diseñado por Evan Czaplicki, ha tenido desde sus inicios un fuerte énfasis en la depuración y trazabilidad del código. La función `Debug.log` ha estado disponible desde las primeras versiones para facilitar esta tarea.

2. **Alternativas**: En Elm existen otras maneras de depurar además de `Debug.log`, como `Debug.toString` que convierte una estructura de datos en una cadena.

3. **Detalles de Implementación**: La función `Debug.log` imprime la salida en la consola del navegador. Sin embargo, usar `Debug.log` en exceso puede ralentizar la aplicación, debido a que la transformación de datos y la escritura en consola son operaciones costosas.