---
title:                "PHP: Concatenando cadenas"
simple_title:         "Concatenando cadenas"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/php/concatenating-strings.md"
---

{{< edit_this_page >}}

## Por qué

Concatenar strings es una habilidad esencial para cualquier programador de PHP. Al unir varias cadenas de caracteres en una sola, se puede crear texto dinámico y personalizado en una página web. Esto es especialmente útil para mostrar información de una base de datos o para crear mensajes de error con los datos específicos del usuario.

## Cómo hacerlo

La concatenación de strings en PHP es muy simple. Todo lo que se necesita es el operador de concatenación (el punto) y las cadenas de texto que se quieren unir. Por ejemplo:

```PHP
// Cadena 1
$nombre = "Juan";
// Cadena 2
$apellido = "Pérez";
// Concatenar las dos cadenas en una sola
$nombre_completo = $nombre . $apellido;
// Imprimir el resultado
echo $nombre_completo;
```

**Resultado:**

JuanPérez

También se pueden concatenar más de dos cadenas a la vez. Simplemente se tiene que añadir los operadores de concatenación entre cada una de ellas:

```PHP
// Cadena 1
$gusto = "Me gusta";
// Cadena 2
$deporte = "el fútbol";
// Cadena 3
$y = "y";
// Cadena 4
$equipo = "mi equipo favorito es Real Madrid";
// Concatenar las cuatro cadenas en una sola
$frase = $gusto . " " . $deporte . " " . $y . " " . $equipo;
// Imprimir el resultado
echo $frase;
```

**Resultado:**

Me gusta el fútbol y mi equipo favorito es Real Madrid

## Profundizando

La concatenación de strings también se puede utilizar junto con variables para crear mensajes dinámicos. Por ejemplo:

```PHP
// Variable con el nombre del usuario
$nombre = "María";
// Variable con la acción que se quiere realizar
$accion = "leer";
// Mensaje de bienvenida personalizado utilizando la concatenación de strings
echo "¡Bienvenida, " . $nombre . "! Gracias por " . $accion . " nuestro blog.";
```

**Resultado:**

¡Bienvenida, María! Gracias por leer nuestro blog.

Se pueden concatenar todo tipo de cadenas de texto, ya sean palabras simples, números, o incluso otras variables. Esto hace que la concatenación de strings sea una herramienta muy versátil en el desarrollo de aplicaciones web.

## Ver también

- [Documentación de PHP sobre concatenación de strings](https://www.php.net/manual/es/language.operators.string.php)
- [Tutorial de concatenación de strings en PHP](https://www.w3schools.com/php/php_operators.asp)
- [Ejemplos prácticos de concatenación de strings en PHP](https://www.phpzag.com/php-string-concatenation/)