---
title:                "Concatenando cadenas"
html_title:           "PHP: Concatenando cadenas"
simple_title:         "Concatenando cadenas"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/php/concatenating-strings.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

Concatenar cadenas es una técnica comúnmente utilizada por programadores en PHP para combinar varias cadenas de texto en una sola. Esto permite crear mensajes personalizados, generar enlaces dinámicamente y manipular datos de manera eficiente.

## ¿Cómo hacerlo?

Para concatenar cadenas en PHP, se utiliza el operador de concatenación (.) entre las cadenas que se desean combinar. Por ejemplo:

```
<?php
$name = "Juan";
echo "Hola, " . $name . "!";

// Output: Hola, Juan!
?>
```

También se pueden concatenar cadenas con variables, lo cual es útil para crear mensajes personalizados. Por ejemplo:

```
<?php
$username = "Maria";
$age = 32;
echo "El usuario " . $username . " tiene " . $age . " años de edad.";

// Output: El usuario Maria tiene 32 años de edad.
?>
```

## Inmersión profunda

La concatenación de cadenas en PHP ha sido una práctica común desde la creación del lenguaje. Sin embargo, se debe tener en cuenta que esta técnica puede ser ineficiente en situaciones donde se concatenan grandes cantidades de cadenas.

Una alternativa a la concatenación es el uso de la función `sprintf()` que permite dar formato a una cadena de texto utilizando marcadores de posición. Por ejemplo:

```
<?php
$username = "Ana";
$age = 25;
$message = sprintf("Hola, %s. ¡Feliz %s cumpleaños!", $username, $age);
echo $message;

// Output: Hola, Ana. ¡Feliz 25 cumpleaños!
?>
```

Otra técnica utilizada por algunos programadores es la interpolación de cadenas, que consiste en colocar variables directamente dentro de una cadena utilizando comillas dobles. Por ejemplo:

```
<?php
$name = "Carlos";
echo "Hola, $name!";

// Output: Hola, Carlos!
?>
```

Es importante tener en cuenta que la interpolación de cadenas sólo funciona con comillas dobles y puede generar errores si se utilizan comillas simples.

## Ver también

- [Documentación oficial de PHP sobre concatenación de cadenas](https://www.php.net/manual/es/language.operators.string.php)
- [Tutorial de concatenación de cadenas en PHP](https://www.w3schools.com/php/php_string_concat.asp)