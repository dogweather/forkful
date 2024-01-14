---
title:    "PHP: Comprobando si existe un directorio"
keywords: ["PHP"]
---

{{< edit_this_page >}}

## Por qué
Revisar si existe un directorio es una tarea común en la programación PHP. Puede ser necesario para asegurarse de que se está trabajando en el lugar correcto o para crear nuevos directorios si no existen. En este blog post, aprenderás cómo verificar si un directorio existe en PHP y profundizarás en el proceso.

## Cómo hacerlo

La mejor forma de verificar si un directorio existe en PHP es utilizando la función `is_dir()`, que devuelve un valor booleano `true` si el directorio existe y `false` si no existe. Aquí tienes un ejemplo de cómo se puede usar esta función en un if statement:

```PHP
if (is_dir("mi_directorio")) {
  echo "El directorio existe";
} else {
  echo "El directorio no existe";
}
```

El código de arriba imprimirá "El directorio existe" si el directorio llamado "mi_directorio" existe en la ruta actual.

## Deep Dive

Además de utilizar la función `is_dir()`, también puedes utilizar la función `file_exists()` para verificar si un directorio existe. La diferencia entre ambas es que `is_dir()` solo se aplica a directorios, mientras que `file_exists()` se puede usar para verificar la existencia de cualquier tipo de archivo, ya sea un directorio, un archivo de texto o una imagen.

En el caso de que necesites manipular el contenido de un directorio, puedes utilizar la función `scandir()` para obtener una lista de todos los archivos y subdirectorios dentro de un directorio específico. Aquí tienes un ejemplo:

```PHP
$directorio = scandir("mi_directorio");
echo "Los archivos en el directorio son: " . implode(", ", $directorio);
```

Este código imprimirá una lista separada por comas de todos los archivos y subdirectorios dentro de "mi_directorio".

## Ver también

- [Función is_dir() en PHP](https://www.php.net/manual/es/function.is-dir.php)
- [Función file_exists() en PHP](https://www.php.net/manual/es/function.file-exists)
- [Función scandir() en PHP](https://www.php.net/manual/es/function.scandir.php)