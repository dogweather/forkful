---
title:    "Gleam: Impresión de salida de depuración"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/gleam/printing-debug-output.md"
---

{{< edit_this_page >}}

## Por qué imprimir salida de depuración en Gleam

La impresión de salida de depuración es una herramienta útil para entender el funcionamiento interno de un programa en Gleam. Al mostrar información adicional durante la ejecución, puede ayudarte a identificar y solucionar problemas en tu código.

## Cómo hacerlo

Para imprimir salida de depuración en Gleam, puedes utilizar la función `io.format`. Esta función toma dos argumentos: una cadena de formato y una lista de valores. Aquí hay un ejemplo:

```Gleam
io.format("El resultado de la suma de 1 y 2 es {}", [(1 + 2)])
```

La salida de este código sería:

```
El resultado de la suma de 1 y 2 es 3
```

Puedes utilizar diferentes tipos de datos en la cadena de formato, como enteros, cadenas de texto, booleanos o incluso estructuras de datos complejas. También puedes utilizar la función `debug.inspect` para imprimir información detallada sobre una variable o valor específico.

## Profundizando

La salida de depuración puede ser especialmente útil al trabajar con funciones o patrones más complejos. Puedes imprimir valores en diferentes puntos de una función para entender mejor cómo se están procesando los datos. También puedes utilizar la salida de depuración en conjunto con pruebas unitarias para encontrar errores en tu código.

Sin embargo, es importante recordar eliminar o comentar cualquier salida de depuración antes de enviar tu código a producción, ya que puede sobrecargar el programa y afectar su rendimiento.

## Ver también

- [Documentación oficial de Gleam sobre salida de depuración](https://gleam.run/core/io.html#format)
- [Tutorial de Gleam para principiantes](https://gleam.run/tutorials/getting-started.html)
- [Ejemplos de código en Gleam](https://github.com/gleam-lang/gleam/tree/master/examples)