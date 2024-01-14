---
title:    "Gleam: Imprimiendo salida de depuración"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## ¿Por qué imprimir salida de depuración en Gleam?

Imprimir la salida de depuración en Gleam puede ser una herramienta útil para detectar y solucionar problemas en tu código. Al imprimir información detallada sobre las variables y resultados de una función, puedes identificar rápidamente dónde se están produciendo errores y cómo solucionarlos.

## Cómo hacerlo

Para imprimir salida de depuración en Gleam, puedes utilizar la función `debug()` seguida de la información que deseas imprimir. Por ejemplo:

```
Gleam debug("El valor de x es", x)
```

Este código imprimirá la cadena "El valor de x es" seguida del valor de la variable `x` en la consola de depuración.

También puedes imprimir la salida de depuración en diferentes niveles de detalle utilizando la función `debug!()` y especificando un nivel de depuración como argumento. Por ejemplo:

```
Gleam debug!(2, "El valor de x es", x)
```

Este código imprimirá la cadena "El valor de x es" seguida del valor de la variable `x` solo si se especifica un nivel de depuración de 2 o superior.

## Inmersión profunda

Además de imprimir valores de variables, también puedes usar la función `debug()` para imprimir mensajes de error y trazas de ejecución. Esto es especialmente útil cuando se utiliza con la función `try()`, que captura y maneja errores en Gleam.

Además, puedes utilizar la función `debug!()` para imprimir cadena de caracteres Unicode y crear una salida de depuración más visual y legible.

## Ver también
- Documentación oficial de Gleam sobre la función `debug()`: [https://gleam.run/documentation/stdlib/debug/](https://gleam.run/documentation/stdlib/debug/)
- Artículo sobre depuración en Gleam en el blog de Codeship: [https://blog.trysmudford.com/debugging-gleam-code/](https://blog.trysmudford.com/debugging-gleam-code/)
- Tutorial interactivo sobre la depuración en Gleam en el sitio web de LoshTechies: [https://loshtechies.com/2018/06/07/gleam-tutorial-11-debugging/](https://loshtechies.com/2018/06/07/gleam-tutorial-11-debugging/)