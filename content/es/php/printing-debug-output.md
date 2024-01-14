---
title:                "PHP: Imprimiendo salida de depuración"
programming_language: "PHP"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/php/printing-debug-output.md"
---

{{< edit_this_page >}}

## Por qué imprimir la salida de depuración es importante

Imprimir la salida de depuración en tus scripts de PHP es una herramienta crucial para encontrar y solucionar errores en tu código. Con la ayuda de la salida de depuración, puedes ver información sobre variables, funciones, y cualquier otra cosa que estés rastreando en tu script. Esto te permite identificar y resolver problemas en tu código de manera eficiente.

## Cómo imprimir la salida de depuración

La impresión de la salida de depuración es fácil de implementar en PHP. Simplemente agrega la función `print_r()` o `var_dump()` antes o después de la sección de código que deseas rastrear. Estas funciones mostrarán información detallada sobre las variables o funciones en cuestión.

```PHP
<?php
$frutas = array("manzana", "naranja", "plátano");

print_r($frutas);
// salida: Array ( [0] => manzana [1] => naranja [2] => plátano )

var_dump($frutas);
// salida: array(3) { [0]=> string(6) "manzana" [1]=> string(6) "naranja" [2]=> string(7) "plátano" }
?>
```

También puedes utilizar la sentencia `echo` para imprimir mensajes de depuración en tu código. Sin embargo, esto solo te dará una salida de texto simple en lugar de una descripción detallada del estado de tus variables.

```PHP
<?php
$numero = 5;

echo "El valor de \$numero es: " . $numero;
// salida: El valor de $numero es: 5
?>
```

## Profundizando en la impresión de salida de depuración

Además de `var_dump()` y `print_r()`, PHP también ofrece otras funciones de depuración como `debug_backtrace()` y `debug_print_backtrace()`. Estas funciones te permiten rastrear la ejecución de tu código, ver la pila de llamadas de funciones y encontrar la fuente de posibles errores.

También puedes personalizar la salida de depuración agregando argumentos a las funciones de depuración. Por ejemplo, puedes utilizar `var_dump($variable, true)` para guardar la salida en una variable en lugar de imprimirla directamente en la pantalla.

¡No subestimes el poder de la impresión de salida de depuración en tus scripts de PHP!

## Ver también

- [Documentación de PHP sobre la impresión de salida de depuración](https://www.php.net/manual/es/function.print-r.php)
- [Guía de depuración para principiantes en PHP](https://www.taniarascia.com/how-to-debug-php/)
- [Depuración de código PHP con VS Code](https://code.visualstudio.com/docs/editor/debugging)