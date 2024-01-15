---
title:                "Uniendo cadenas de texto"
html_title:           "PHP: Uniendo cadenas de texto"
simple_title:         "Uniendo cadenas de texto"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/php/concatenating-strings.md"
---

{{< edit_this_page >}}

# Por qué

¿Alguna vez te has encontrado en la situación en la que necesitas combinar varias cadenas de texto para crear una sola? Esto es común en la programación, ya sea para mostrar información en un sitio web o para manipular datos en una base de datos. Concatenar strings en PHP es una habilidad básica que te permitirá controlar de manera efectiva el contenido de tus programas.

# Cómo hacerlo
Para concatenar strings en PHP, utilizamos el operador ".", que nos permite unir una o más cadenas de texto. Por ejemplo:
```PHP
$nombre = "Juan";
$apellido = "Pérez";
$nombre_completo = $nombre . " " . $apellido;
echo $nombre_completo; // Salida: Juan Pérez
```
En este ejemplo, utilizamos el operador "." para unir el nombre y el apellido en una sola variable llamada $nombre_completo. También podemos combinar strings con variables y valores numéricos:
```PHP
$edad = 30;
$informacion = $nombre . " " . $apellido . " tiene " . $edad . " años.";
echo $informacion; // Salida: Juan Pérez tiene 30 años.
```
Otra forma de concatenar strings en PHP es mediante el uso de la función `sprintf()`, que nos permite formatear cadenas de texto con marcadores de posición para insertar variables. Por ejemplo:
```PHP
$precio = 10.50;
$cantidad = 2;
$total = sprintf("El total es: $%.2f", $precio * $cantidad);
echo $total; // Salida: El total es: $21.00
```
En este caso, utilizamos el marcador de posición "%.2f" para insertar el valor de la multiplicación entre $precio y $cantidad con dos decimales y el símbolo de dólar delante.

# Profundizando en la concatenación de strings
Hay algunas cosas que debes tener en cuenta al concatenar strings en PHP. 

En primer lugar, debes asegurarte de que las cadenas que estás uniendo estén encerradas entre comillas dobles ("") o comillas simples (''). Si no las utilizas, PHP no podrá diferenciar las cadenas de las variables y lanzará un error.

Además, debes tener en cuenta que al usar el operador ".", estás creando una nueva cadena cada vez que lo utilizas. Esto puede ser ineficiente en términos de memoria, especialmente si estás concatenando grandes cantidades de texto. En estos casos, es mejor utilizar la función `sprintf()`.

También es importante tener en cuenta el orden en el que se concatenan las cadenas. Por ejemplo, si tienes una cadena que contiene la palabra "PHP", y luego la concatenas con otra que tiene la palabra "programar", el resultado será "PHPprogramar" en lugar de "programarPHP". Por lo tanto, es importante tener en cuenta el orden para obtener el resultado deseado.

# Ver también
- [Documentación de PHP sobre concatenación de strings](https://www.php.net/manual/es/language.operators.string.php)
- [Ejemplos de concatenación de strings en PHP](https://www.w3schools.com/php/php_string_concat.asp)
- [Tutorial de concatenación de strings en PHP](https://www.hostinger.es/tutoriales/concatenar-strings-php/)