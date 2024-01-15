---
title:                "Imprimiendo la salida de depuración"
html_title:           "PHP: Imprimiendo la salida de depuración"
simple_title:         "Imprimiendo la salida de depuración"
programming_language: "PHP"
category:             "PHP"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/php/printing-debug-output.md"
---

{{< edit_this_page >}}

## Por qué

¿Alguna vez te has encontrado con un error en tu código y no has sido capaz de averiguar qué está sucediendo? ¿Te gustaría tener una forma de imprimir información útil durante la ejecución de tu programa para poder depurarlo más fácilmente? ¡Pues imprimir output de debug en PHP es la solución perfecta para ti!

## Cómo imprimir output de debug en PHP

¡Es muy sencillo! Solo tienes que usar la función `print_r()` o `var_dump()` para imprimir una representación legible de cualquier variable o expresión en PHP. Veamos un ejemplo:

```PHP
$array = [1, 2, 3, 4];
print_r($array);
```

Este código imprimirá lo siguiente en la pantalla:

```
Array
(
    [0] => 1
    [1] => 2
    [2] => 3
    [3] => 4
)
```

También puedes imprimir directamente una cadena de texto o un número utilizando `echo`. Por ejemplo:

```PHP
$nombre = "Juan";
echo "Hola " . $nombre;
```

Esto imprimirá:

```
Hola Juan
```

Pero ¿qué pasa si quieres imprimir información específica de una variable? En lugar de imprimir todo el contenido de la variable con `print_r()` o `var_dump()`, puedes utilizar la función `printf()` y especificar un formato para la salida. Por ejemplo:

```PHP
$numero = 20;
printf("El número es %d", $numero);
```

Esto imprimirá:

```
El número es 20
```

Existen muchas otras funciones y técnicas para imprimir output de debug en PHP, así que te animo a que investigues un poco más para encontrar la opción que mejor se adapte a tus necesidades.

## Deep Dive

Además de las funciones mencionadas anteriormente, PHP ofrece una gran variedad de herramientas para imprimir output de debug de una forma más avanzada. Puedes imprimir información sobre errores en el código utilizando `error_log()`, o escribir mensajes en el terminal con `cli_set_process_title()`. También es posible utilizar la extensión `Xdebug` para realizar depuración remota y obtener información detallada sobre el proceso de ejecución de tu programa.

Pero ten en cuenta que imprimir demasiado output de debug puede ralentizar la ejecución de tu código, así que es importante utilizarlo con moderación y eliminarlo una vez que hayas resuelto el problema.

## Ver también

- [Documentación oficial de PHP sobre debugging](https://www.php.net/manual/es/debugger.php)
- [Artículo de Blog - Depurando en PHP: Técnicas y Herramientas](https://blog.uptodown.com/depurar-php-tecnicas-herramientas/)
- [Curso de Depuración en PHP en línea - Udemy](https://www.udemy.com/course/depuracion-php/)