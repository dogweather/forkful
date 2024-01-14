---
title:    "PHP: Comprobación de existencia de un directorio"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/php/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# Por qué verificar si un directorio existe en PHP

En la programación PHP, puede ser muy útil verificar si un directorio existe antes de realizar operaciones en él. Esto puede ayudar a evitar errores y asegurarse de que su código se esté ejecutando correctamente.

## Cómo hacerlo

Para verificar si un directorio existe en PHP, puede utilizar la función `is_dir()` seguida del nombre del directorio que desea verificar entre paréntesis.

```PHP
$directorio = "archivos/";
if (is_dir($directorio)) {
    echo "El directorio $directorio existe";
} else {
    echo "El directorio $directorio no existe";
}
```

Si el directorio "archivos" existe, el código anterior imprimirá "El directorio archivos existe". De lo contrario, imprimirá "El directorio archivos no existe".

## Profundizando

Además de la función `is_dir()`, existen varias formas de verificar si un directorio existe y realizar operaciones en él. Estos incluyen:

- La función `file_exists()` que puede verificar tanto directorios como archivos.
- La función `scandir()` que devuelve una lista de archivos y subdirectorios en un directorio especificado.
- El uso de la clase `Directory` para trabajar con directorios y archivos en un objeto orientado a objetos.

También es importante tener en cuenta que al trabajar con directorios en PHP, es necesario tener en cuenta la estructura del sistema de archivos en el que se está trabajando y los permisos de acceso necesarios para realizar operaciones en determinados directorios.

## Ver también

- [Documentación oficial de PHP sobre la función is_dir()](https://www.php.net/manual/es/function.is-dir.php)
- [Tutorial sobre el manejo de archivos y directorios en PHP](https://www.tutorialspoint.com/php/php_file_system.htm)
- [Otra forma de verificar si un directorio existe en PHP](https://www.geeksforgeeks.org/php-check-whether-a-directory-exists-or-not/)

¡Con esta información, ahora puedes asegurarte de que los directorios en tu código PHP existan antes de realizar operaciones en ellos!