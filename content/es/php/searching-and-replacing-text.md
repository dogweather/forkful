---
title:    "PHP: Buscando y reemplazando texto"
keywords: ["PHP"]
---

{{< edit_this_page >}}

## Por qué

Reemplazar textos es una tarea común en la programación y puede ser útil en varias situaciones, como cambiar una palabra mal escrita en un documento o actualizar una cadena de texto en una aplicación. Aprender a realizar esta tarea en PHP te ayudará en tus proyectos futuros y te ahorrará tiempo y esfuerzo en la corrección de errores.

## Cómo hacerlo

El proceso básico para buscar y reemplazar texto en PHP es sencillo y se puede hacer con la función `str_replace()`. Esta función toma 3 parámetros: el texto que deseas buscar, el texto que quieres reemplazar y la cadena donde se realizará la búsqueda. A continuación, se muestra un ejemplo de cómo reemplazar la palabra "hola" con "hello" en una cadena de texto:

```PHP
$cadena = "¡Hola mundo!";
$cadena = str_replace("hola", "hello", $cadena);
echo $cadena;
```

La salida de este código sería "¡Hello mundo!". Puedes aplicar esta función a cualquier tipo de cadena, ya sea una variable o un texto directo.

## Profundizando

La función`str_replace()` no solo reemplaza palabras completas, sino que también puede funcionar con parte de una cadena de texto. Por ejemplo, si solo deseas cambiar la primera letra de una palabra, puedes hacerlo utilizando `str_replace` de esta manera:

```PHP
$cadena = "Hola amigo";
$cadena = str_replace("h", "H", $cadena);
echo $cadena;
```

La salida sería "Hola amigo", ya que solo se reemplazó la primera letra "h" con "H".

También puedes utilizar esta función para buscar y reemplazar múltiples palabras a la vez, simplemente agregando los valores entre corchetes y separándolos por comas. Por ejemplo, si deseas reemplazar tanto "hola" como "amigo" en la cadena anterior, puedes hacerlo de la siguiente manera:

```PHP
$cadena = "Hola amigo";
$cadena = str_replace(array("hola", "amigo"), array("hello", "friend"), $cadena);
echo $cadena;
```

La salida sería "Hello friend".

Otra función útil para reemplazar texto es `str_ireplace()`, que funciona de la misma manera que `str_replace()` pero sin tener en cuenta las mayúsculas y minúsculas. Esto significa que puedes reemplazar "hola" por "Hello" y aún así funcionará en una cadena con "Hola".

## Ver también

- La documentación oficial de PHP sobre [str_replace()](https://www.php.net/manual/es/function.str-replace.php)
- Una guía más detallada sobre [cómo reemplazar texto en PHP](https://www.w3schools.com/php/func_string_str_replace.asp)
- Otros [ejemplos prácticos](https://www.php.net/manual/es/function.str-replace.php#122686) de uso de `str_replace()` en la comunidad de PHP.