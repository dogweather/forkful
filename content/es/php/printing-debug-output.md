---
title:                "Impresión de salida de depuración"
html_title:           "PHP: Impresión de salida de depuración"
simple_title:         "Impresión de salida de depuración"
programming_language: "PHP"
category:             "PHP"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/php/printing-debug-output.md"
---

{{< edit_this_page >}}

## Qué y por qué?

Imprimir la salida de depuración es una herramienta útil para los programadores de PHP. Consiste en mostrar información sobre el código en tiempo de ejecución, lo que facilita la identificación de errores y el seguimiento del flujo del programa.

## Cómo:

El siguiente ejemplo muestra cómo utilizar la función "var_dump" para imprimir la información de depuración en PHP:

```PHP
<?php
$variable = "Hola mundo";
var_dump($variable);
?>
```
La salida será: string(10) "Hola mundo"

También se puede utilizar la función "print_r" para imprimir la información de una manera más legible:
```PHP
<?php
$array = array("Manzana", "Banana", "Cereza");
print_r($array);
?>
```
La salida será: Array ( [0] => Manzana [1] => Banana [2] => Cereza )

## Profundizando:

La salida de depuración se ha convertido en una práctica común entre los programadores, ya que permite ahorrar tiempo en la identificación y resolución de errores. Además de las funciones "var_dump" y "print_r", existen otras formas de imprimir la salida de depuración, como utilizar la función "echo" o "printf". Sin embargo, es importante tener en cuenta que la impresión de salida de depuración no debería ser utilizada en una versión final del código, ya que puede ralentizar el rendimiento del programa y exponer datos sensibles.

## Ver también:

Para más información sobre la impresión de salida de depuración, se puede consultar la documentación oficial de PHP en el siguiente enlace: https://www.php.net/manual/es/function.var-dump.php. También se pueden encontrar tutoriales y ejemplos en línea que pueden ser útiles para profundizar en esta técnica de depuración.