---
title:    "PHP: Eliminando caracteres que coinciden con un patrón"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/php/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## ¿Por qué eliminar caracteres que coinciden con un patrón?

A veces, mientras se trabaja con cadenas de texto en PHP, puede ser necesario eliminar ciertos caracteres que coinciden con un patrón específico. Ya sea para limpiar datos o para realizar tareas específicas en una cadena de texto, esta es una habilidad útil que todo programador debe tener.

## Cómo hacerlo en PHP

Para eliminar caracteres que coinciden con un patrón en PHP, primero debemos utilizar la función `preg_replace()` que nos permite realizar búsquedas mediante expresiones regulares. Esta función toma tres argumentos: el patrón a buscar, el texto en el que buscar y el texto de reemplazo (en caso de ser necesario).

Un ejemplo sencillo sería eliminar todos los números de una cadena de texto. Para ello, usaríamos el patrón `/[0-9]/` para indicar que queremos buscar cualquier número del 0 al 9. Luego, utilizamos el espacio en blanco como texto de reemplazo ya que queremos eliminar los números y no reemplazarlos por nada.

```
<?php 

$texto = "La contraseña es 1234";
$nueva_contraseña = preg_replace("/[0-9]/", "", $texto);

echo $nueva_contraseña;

// Output: La contraseña es 

?>
```

También podemos usar caracteres comodín para eliminar patrones más complejos. Por ejemplo, si queremos eliminar todos los caracteres que no sean letras, números ni espacios, podemos usar el patrón `/[^a-zA-Z0-9\s]/`. El carácter `^` al principio del patrón indica que queremos eliminar todo lo que no esté dentro del patrón.

```
<?php 

$texto = "¡Hola, ¿cómo estás? 123";
$nuevo_texto = preg_replace("/[^a-zA-Z0-9\s]/", "", $texto);

echo $nuevo_texto;

// Output: Hola cómo estás 123

?>
```

## Profundizando en la eliminación de caracteres

Si queremos ser más específicos con nuestra eliminación de caracteres, podemos utilizar grupos de captura en nuestras expresiones regulares. Esto nos permitirá reemplazar solo ciertas partes del texto que coinciden con el patrón en lugar de toda la cadena.

Por ejemplo, si queremos eliminar solo los códigos de área de un número de teléfono en formato (###) ###-####, podemos usar el patrón `/\((\d{3})\)/` y reemplazarlo por una cadena vacía. Esto eliminará solo los paréntesis y el código de área, dejando el resto del número intacto.

```
<?php 

$numero = "(555) 123-4567";
$nuevo_numero = preg_replace("/\((\d{3})\)/", "", $numero);

echo $nuevo_numero;

// Output: 555123-4567

?>
```

Existen muchas otras posibilidades y casos de uso para la eliminación de caracteres que coinciden con un patrón. Al dominar esta habilidad, podemos trabajar con cadenas de texto de manera más eficiente y realizar tareas complejas en poco tiempo.

## Ver también

- [Documentación de la función `preg_replace()` en PHP](https://www.php.net/manual/es/function.preg-replace.php)
- [Guía de expresiones regulares en PHP](https://www.w3schools.com/php/php_regex.asp)