---
title:    "PHP: Uniendo cadenas de texto"
keywords: ["PHP"]
---

{{< edit_this_page >}}

## Por qué

Al programar en PHP, a menudo nos encontramos con la necesidad de combinar o unir varias cadenas de texto juntas. Esto se conoce como concatenación y es una práctica común en la programación. La concatenación de cadenas nos permite crear cadenas más complejas y personalizadas para su uso en nuestras aplicaciones web o scripts.

## Cómo hacerlo

En PHP, podemos concatenar cadenas de texto utilizando el operador `.` (punto). Por ejemplo, si queremos combinar el nombre y apellido de una persona para crear una cadena con su nombre completo, podríamos escribir lo siguiente:

```PHP
$nombre = "María";
$apellido = "Rodríguez";
$nombre_completo = $nombre . " " . $apellido;

echo $nombre_completo;
```

Este código imprimirá "María Rodríguez" en la pantalla. Como puedes ver, hemos utilizado el operador `.` para unir las dos variables `$nombre` y `$apellido`, agregando también un espacio en blanco entre ellas.

También podemos concatenar una cadena con un número u otra variable de diferentes tipos de datos. Por ejemplo:

```PHP
$cantidad = 100;
$mensaje = "El total es: " . $cantidad;

echo $mensaje;
```

Este código imprimirá "El total es: 100". Aquí, hemos utilizado el operador `.` para unir la cadena "El total es: " con el valor de la variable `$cantidad`.

## Profundizando

Además del operador `.` para la concatenación, también existe otra forma de unir cadenas en PHP: la función `sprintf()`. Esta función nos permite especificar un formato para nuestra cadena y luego insertar variables o valores en él.

Por ejemplo, si queremos crear una cadena con un saludo personalizado y la fecha actual, podríamos hacerlo de la siguiente manera con `sprintf()`:

```PHP
$nombre = "Juan";
$hoy = date("d/m/Y");
$mensaje = sprintf("¡Hola %s! Hoy es %s", $nombre, $hoy);

echo $mensaje;
```

La salida sería "¡Hola Juan! Hoy es 07/09/2021". Aquí, `%s` es utilizado como marcador de posición para nuestras variables, y la función `sprintf()` se encarga de insertar los valores correspondientes en su lugar.

## Ver también

- [Documentación de concatenación de cadenas en PHP](https://www.php.net/manual/es/language.operators.string.php)
- [Ejemplos prácticos de concatenación de cadenas](https://codeburst.io/string-concatenation-and-interpolation-in-php-c107ec4b1a1f)
- [Más sobre la función `sprintf()`](https://www.php.net/manual/es/function.sprintf.php)