---
title:                "Comprobando si existe un directorio"
html_title:           "PHP: Comprobando si existe un directorio"
simple_title:         "Comprobando si existe un directorio"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/php/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Por qué

A veces, cuando estamos escribiendo código, es necesario saber si una carpeta existe o no en nuestro sistema de archivos. Esto puede ser útil para evitar errores o para llevar a cabo ciertas acciones en función de si la carpeta existe o no. En esta sección, aprenderemos cómo verificar si una carpeta existe utilizando PHP.

## Cómo hacerlo

La función que nos permitirá verificar si una carpeta existe o no se llama `file_exists()`. Esta función toma una ruta de archivo como argumento y devuelve `true` si el archivo o carpeta existe, y `false` si no existe. Echemos un vistazo a un ejemplo de código:

```PHP
if(file_exists("ruta/a/carpeta")){
  echo "La carpeta existe";
} else {
  echo "La carpeta no existe";
}
```

Si la ruta especificada apunta a una carpeta existente, el código imprimirá "La carpeta existe". De lo contrario, imprimirá "La carpeta no existe".

También podemos utilizar la función `is_dir()` para verificar si la ruta especificada es una carpeta o no. Esta función devuelve `true` si la ruta es una carpeta y `false` si no lo es. Echemos un vistazo a un ejemplo de código:

```PHP
if(is_dir("ruta/a/carpeta")){
  echo "La ruta apunta a una carpeta";
} else {
  echo "La ruta no apunta a una carpeta";
}
```

Si la ruta especificada es una carpeta, el código imprimirá "La ruta apunta a una carpeta". De lo contrario, imprimirá "La ruta no apunta a una carpeta".

## Profundizando

Puede que te estés preguntando qué sucede si la carpeta que estamos verificando no está en la ruta que especificamos, sino en una ruta diferente. En ese caso, necesitaremos utilizar la función `realpath()`, que nos permite obtener la ruta real de la carpeta. Por ejemplo, si tenemos una carpeta llamada "carpeta" en la ruta "ruta/a/carpeta", pero queremos verificar si existe en la ruta "ruta/b/carpeta", necesitamos utilizar `realpath()` como sigue:

```PHP
if(file_exists(realpath("ruta/b/carpeta"))){
  echo "La carpeta existe";
} else {
  echo "La carpeta no existe";
}
```

Además, si necesitas crear una nueva carpeta en una ruta específica, puedes utilizar la función `mkdir()`, que toma como argumento la ruta de la carpeta que quieres crear.

## Ver también

- [Documentación de PHP para file_exists()](https://www.php.net/manual/es/function.file-exists.php)
- [Documentación de PHP para is_dir()](https://www.php.net/manual/es/function.is-dir.php)
- [Documentación de PHP para realpath()](https://www.php.net/manual/es/function.realpath.php)
- [Documentación de PHP para mkdir()](https://www.php.net/manual/es/function.mkdir.php)