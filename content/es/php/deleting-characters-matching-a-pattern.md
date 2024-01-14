---
title:                "PHP: Borrando caracteres que coinciden con un patrón"
programming_language: "PHP"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/php/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## ¿Por qué borrar caracteres que coinciden con un patrón?

A veces, al manipular cadenas de texto en PHP, es necesario eliminar caracteres que coinciden con un patrón específico. Esto puede ser parte de una validación de datos o una tarea de edición de texto. A continuación, te mostramos cómo hacerlo en unos sencillos pasos.

## Cómo hacerlo

En PHP, podemos utilizar la función `preg_replace()` para eliminar caracteres que coinciden con un patrón en una cadena de texto. Esta función toma tres argumentos: el patrón de búsqueda, el nuevo texto y la cadena de texto original. Por ejemplo, si queremos eliminar todos los dígitos de una cadena de texto, podríamos usar el siguiente código:

```PHP
$string = "¡H0l4, b13nv3n1d0!";
$new_string = preg_replace("/\d/", "", $string);
echo $new_string;
```

El resultado sería:

```
¡Hl, bnvnd!;
```

Aquí, el patrón `/\d/` coincide con cualquier dígito en la cadena de texto y la función `preg_replace()` los reemplaza con una cadena vacía. Podemos adaptar este código para que coincida con cualquier patrón que necesitemos eliminar de nuestra cadena.

## Profundizando

La función `preg_replace()` de PHP utiliza expresiones regulares para realizar la búsqueda y reemplazo de patrones en una cadena de texto. Las expresiones regulares son una forma poderosa de buscar y manipular texto que puede ser útil en muchas aplicaciones de programación.

En nuestro ejemplo anterior, utilizamos `\d` como patrón para eliminar los dígitos. Sin embargo, también existe una amplia gama de caracteres especiales que podemos utilizar para crear patrones más específicos. Por ejemplo, el carácter `.` coincide con cualquier carácter individual, mientras que `\w` coincide con cualquier carácter alfanumérico. Puedes consultar la documentación oficial de PHP para obtener una lista completa de los caracteres especiales disponibles.

Otra función útil para borrar caracteres coincidentes es `preg_replace_callback()`, que nos permite ejecutar una función personalizada para cada coincidencia que se encuentre en la cadena de texto original. Esto puede ser útil si queremos realizar algún proceso específico con cada coincidencia.

## Ver también

- [Documentación oficial de PHP sobre `preg_replace()`](https://www.php.net/manual/es/function.preg-replace)
- [Expresiones regulares en PHP](https://www.php.net/manual/es/book.pcre.php)
- [Pruebas de expresiones regulares en línea para PHP](https://regex101.com/library)