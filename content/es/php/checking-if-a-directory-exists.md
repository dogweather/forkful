---
title:                "Verificando si existe un directorio"
html_title:           "PHP: Verificando si existe un directorio"
simple_title:         "Verificando si existe un directorio"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/php/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Qué y Por qué?
Comprobar si un directorio existe es una forma común para que los programadores se aseguren de que su código pueda acceder a ciertas rutas de archivos o directorios. Esto es especialmente importante en aplicaciones web, donde se necesita acceder a diferentes recursos o datos almacenados en el servidor. Verificar la existencia de un directorio antes de realizar operaciones en él también evita errores y fallas en el código.

## Cómo hacerlo:

```
<?php
if (is_dir($ruta_directorio)) { // Comprueba si la ruta del directorio existe
    echo "El directorio existe";
}else{
    echo "El directorio no existe";
}
```

El código anterior utiliza la función `is_dir()` de PHP para verificar si la ruta de un directorio dado existe. En caso de que la ruta sea válida, la función devuelve `true`, de lo contrario devuelve `false`. Luego, podemos utilizar un simple condicional para imprimir un mensaje en consecuencia.

```
<?php
$ruta_directorio = "archivos/";

// Comprueba si la ruta del directorio existe
if (is_dir($ruta_directorio)) {
    // Abre el directorio
    if ($directorio = opendir($ruta_directorio)) {
        // Recorre todos los archivos en el directorio
        while (($archivo = readdir($directorio)) !== false) {
            // Solo muestra los archivos (y no los directorios)
            if (!is_dir($archivo)) {
                echo $archivo . "<br>";
            }
        }
        // Cierra el directorio
        closedir($directorio);
    }
}
```

En este ejemplo, además de verificar si el directorio existe, también mostramos el contenido de ese directorio mediante un bucle `while` y la función `readdir()`. Tenga en cuenta que, en este caso, el directorio solo se abrirá si se ha confirmado previamente su existencia.

## Profundizando:

Antes de la versión 4.0.6 de PHP, se utilizaba la función `realpath()` para verificar la existencia de un directorio. Sin embargo, esta función tiene un comportamiento diferente al de `is_dir()`, ya que también resuelve cualquier enlace simbólico en la ruta del directorio. A partir de la versión 4.0.6, la función `is_dir()` se incorporó al núcleo de PHP para proporcionar un método más consistente y confiable para verificar la existencia de un directorio.

Existen otras formas de verificar si un directorio existe, como utilizar la función `file_exists()` o la clase `Directory`. Sin embargo, la función `is_dir()` sigue siendo la opción recomendada debido a su simplicidad y eficiencia.

## Ver también:

- [Documentación oficial de PHP sobre la función `is_dir()`](https://www.php.net/manual/es/function.is-dir.php)
- [Documentación oficial de PHP sobre la función `readdir()`](https://www.php.net/manual/es/function.readdir.php)