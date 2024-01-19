---
title:                "Imprimiendo salida de depuración"
html_title:           "Arduino: Imprimiendo salida de depuración"
simple_title:         "Imprimiendo salida de depuración"
programming_language: "PHP"
category:             "PHP"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/php/printing-debug-output.md"
---

{{< edit_this_page >}}

## Qué y por qué?
La impresión de salida de depuración en PHP es una táctica utilizada para realizar seguimientos y descubrir errores en el código. Esta es una herramienta valiosa ya que permite a los programadores entender mejor su programa al ver su comportamiento en tiempo real.

## Cómo hacerlo:

La función básica en PHP para la depuración de salida es `var_dump()`. Esta función muestra información estructurada sobre una variable o expresión.

```PHP
<?php 
$test = "Hola Mundo";
# mostrarás tipo y valor de la variable test
var_dump($test);

# salida
string(10) "Hola Mundo"
?>
```

Para casos más avanzados, puedes usar una biblioteca como Xdebug, que proporciona depuración paso a paso y traza de llamadas.

## Un vistazo más profundo:

`var_dump()` ha sido parte de PHP desde la versión 4.0.0. Aunque otros métodos, como `print_r()` y `var_export()`, pueden hacer un trabajo similar, `var_dump()` es único porque también muestra el tipo y tamaño de la variable.

Las bibliotecas de terceros como Xdebug ofrecen una funcionalidad de depuración más robusta y detallada. Con Xdebug, puedes hacer seguimientos de pila, mostrar scripts de tiempo de ejecución y también proporciona una interfaz DBGp para herramientas de depuración remotas.

## Ver también:

1. [Documentación oficial PHP var_dump()](https://www.php.net/manual/es/function.var-dump.php) - para una exploración detallada de cómo var_dump trabaja.
2. [Xdebug](https://xdebug.org/) - página oficial, con guías de instalación y configuración.
3. [Introducción a la depuración de PHP](http://php.net/manual/es/debugger.php) - para un resumen más amplio sobre depuración en PHP.