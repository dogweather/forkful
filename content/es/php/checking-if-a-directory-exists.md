---
title:                "PHP: Verificando si existe un directorio"
simple_title:         "Verificando si existe un directorio"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/php/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Por qué

¿Alguna vez te has preguntado si un directorio específico existe en tu código de PHP? Puede que te hayas encontrado en una situación en la que necesitas asegurarte de que un directorio esté presente antes de realizar ciertas operaciones en tu código. En esta publicación, aprenderemos cómo comprobar si un directorio existe en PHP y por qué es útil hacerlo.

## Cómo hacerlo

Para comprobar si un directorio existe en PHP, podemos utilizar la función `is_dir()` seguida del nombre del directorio como parámetro. Esta función devolverá `true` si el directorio existe y `false` si no existe. Veamos un ejemplo:

```PHP
if (is_dir('imagenes')) {
    echo '¡El directorio existe!';
} else {
    echo 'El directorio no existe';
}
```

Si el directorio "imagenes" existe en el mismo directorio que nuestro archivo PHP, el código de arriba imprimirá "¡El directorio existe!" de lo contrario, imprimirá "El directorio no existe".

## Deep Dive

En PHP, también podemos utilizar la función `file_exists()` para comprobar si un directorio existe. Sin embargo, esta función también puede comprobar si existe un archivo con el mismo nombre que el directorio. Por lo tanto, es importante especificar el directorio en lugar de solo el nombre en la función.

Otra forma de verificar si un directorio existe es utilizando la función `scandir()`, que devuelve una lista de todos los archivos y directorios dentro de un directorio dado. Si el directorio que estamos buscando no aparece en la lista, significa que no existe.

## Véase también

- [Documentación de PHP sobre la función is_dir()](https://www.php.net/manual/es/function.is-dir.php)
- [Más información sobre la función file_exists()](https://www.php.net/manual/es/function.file-exists.php)
- [Ejemplos de la función scandir()](https://www.php.net/manual/es/function.scandir.php)