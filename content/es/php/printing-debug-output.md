---
title:    "PHP: Imprimiendo salida de depuración"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/php/printing-debug-output.md"
---

{{< edit_this_page >}}

## Por qué

¿Alguna vez te has encontrado con un error en tu código y no sabes por dónde empezar a solucionarlo? La impresión de la salida de depuración en PHP puede ser una herramienta útil para ayudarte a entender lo que está sucediendo en tu código y encontrar rápidamente el problema.

## Cómo hacerlo

Hay varias formas de imprimir la salida de depuración en PHP, pero la más común es utilizando la función `print_r()`. Veamos un ejemplo:

```PHP
$mi_array = array('manzana', 'naranja', 'plátano');
print_r($mi_array);
```

La salida de este código será la siguiente:

```
Array
(
    [0] => manzana
    [1] => naranja
    [2] => plátano
)
```

Este ejemplo nos muestra el contenido y estructura de nuestro array de manera clara y legible, lo que puede ser de gran ayuda para encontrar errores o entender cómo está funcionando nuestro código.

Otra función útil es `var_dump()`, que nos muestra no solo el contenido de una variable, sino también su tipo de dato y longitud. Por ejemplo:

```PHP
$nombre = 'Juan';
var_dump($nombre);
```

La salida de este código será la siguiente:

```
string(4) "Juan"
```

Esto nos indica que la variable `$nombre` es una cadena de texto con una longitud de 4 caracteres.

## Profundizando

Además de `print_r()` y `var_dump()`, PHP también ofrece otras funciones de salida de depuración como `debug_backtrace()` y `error_log()`. Estas funciones pueden ser de gran utilidad en situaciones más complejas, como cuando necesitamos rastrear un error específico o guardar los mensajes de error en un archivo.

También es importante mencionar que la impresión de la salida de depuración debe usarse con precaución. No es una buena práctica dejar estas funciones en tu código en producción, ya que pueden revelar información sensible sobre tu aplicación a posibles atacantes. Es mejor utilizarlas solo durante los procesos de desarrollo y pruebas.

## Ver también

- [Funciones de salida de depuración en PHP](https://www.php.net/manual/es/function.debug-backtrace.php)
- [Documentación oficial de PHP sobre depuración](https://www.php.net/manual/es/debugger.php)
- [Artículo sobre depuración de código PHP en producción](https://blog.maestrano.com/debugging-php-apps-in-production/)