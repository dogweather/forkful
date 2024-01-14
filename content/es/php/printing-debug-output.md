---
title:                "PHP: Imprimiendo salida de depuración"
simple_title:         "Imprimiendo salida de depuración"
programming_language: "PHP"
category:             "PHP"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/php/printing-debug-output.md"
---

{{< edit_this_page >}}

## Por qué

La impresión de datos de depuración es una técnica común en la programación PHP. Esta práctica permite a los desarrolladores revisar y solucionar problemas en su código de manera más eficiente. Además, imprimir datos de depuración es una excelente manera de entender el flujo de su programa y encontrar errores ocultos.

## Cómo hacerlo

Para imprimir datos de depuración en PHP, podemos utilizar la función `print_r()` o `var_dump()` dependiendo de lo que queramos imprimir. Por ejemplo, supongamos que tenemos un arreglo llamado `$personas` con información sobre diferentes personas. Podemos imprimir este arreglo de la siguiente manera:

```PHP
<?php
$personas = array(
    array("nombre" => "Juan", "edad" => 25),
    array("nombre" => "María", "edad" => 30),
    array("nombre" => "Pedro", "edad" => 40)
);

print_r($personas);
```

Esto nos dará como resultado:

```
Array
(
    [0] => Array
        (
            [nombre] => Juan
            [edad] => 25
        )

    [1] => Array
        (
            [nombre] => María
            [edad] => 30
        )

    [2] => Array
        (
            [nombre] => Pedro
            [edad] => 40
        )

)
```

Si queremos ver más información sobre las variables, podemos usar `var_dump()` en su lugar. Por ejemplo, si tenemos la siguiente variable:

```PHP
<?php
$precio = 20.50;
```

Podemos imprimir su tipo de dato, su valor y su longitud de la siguiente manera:

```PHP
<?php
var_dump($precio);
```

Esto nos dará como resultado:

```
float(20.5)
```

## Profundizando en la impresión de datos de depuración

Además de `print_r()` y `var_dump()`, también podemos utilizar otras técnicas para imprimir datos de depuración en PHP. Por ejemplo, podemos usar la función `debug_backtrace()` para obtener información sobre la ejecución de nuestro código. También podemos utilizar la extensión Xdebug para imprimir nuestros datos de manera más visual y detallada.

Otra técnica útil es el uso de etiquetas HTML para formatear la salida de nuestros datos de depuración, como `<pre>` para mostrarlos en una fuente de ancho fijo o `<table>` para mostrarlos en una tabla.

Recuerda que es importante realizar una limpieza de tu código antes de ponerlo en producción, eliminando todas las impresiones de datos de depuración que hayas añadido.

## Mira también

- [Documentación de PHP sobre print_r()](https://www.php.net/manual/es/function.print-r.php)
- [Documentación de PHP sobre var_dump()](https://www.php.net/manual/es/function.var-dump.php)
- [Documentación de PHP sobre debug_backtrace()](https://www.php.net/manual/es/function.debug-backtrace.php)
- [Sitio oficial de Xdebug](https://xdebug.org/)