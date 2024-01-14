---
title:                "PHP: Comprobando si existe un directorio"
programming_language: "PHP"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/php/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## ¿Por qué comprobar si existe un directorio en PHP?

Comprobar si un directorio existe antes de realizar cualquier operación en él es una buena práctica de programación. Esto puede evitar errores y asegurar que su código funcione correctamente. Imagine tratar de manipular un archivo o carpeta que no existe, esto podría resultar en una interrupción en el flujo de su programa. Por eso, es importante verificar la existencia de un directorio antes de realizar cualquier acción en él.

## Cómo hacerlo

Para comprobar si un directorio existe en PHP, podemos utilizar la función `is_dir()`. Esta función toma como parámetro la ruta de la carpeta que deseamos verificar y devuelve `true` si existe y `false` si no. Veamos un ejemplo:

```PHP
 $directorio = "archivos";

 if(is_dir($directorio)){
     echo "El directorio $directorio existe.";
 } else {
     echo "El directorio $directorio no existe.";
 }

 // Output: El directorio archivos existe.
```

Además, podemos utilizar la función `is_file()` para comprobar si un archivo específico existe dentro del directorio. Esta función también devuelve `true` o `false` según sea el caso. Veamos un ejemplo:

```PHP
 $directorio = "archivos";
 $archivo = "documento.txt";

 if(is_file($directorio."/".$archivo)){
     echo "El archivo $archivo existe en el directorio $directorio.";
 } else {
     echo "El archivo $archivo no existe en el directorio $directorio.";
 }

 // Output: El archivo documento.txt existe en el directorio archivos.
```

## Profundizando

Para aquellos que deseen investigar más sobre el tema, es importante tener en cuenta que la función `is_dir()` también puede devolver `true` si el parámetro pasado es un archivo y no un directorio. Esto se debe a que la función verifica tanto la existencia de un directorio como de un archivo con ese nombre.

Otra forma de comprobar la existencia de un directorio es mediante la función `file_exists()`, que también devuelve `true` o `false` según la existencia de un archivo o directorio en una ruta específica.

Es importante mencionar que, al manipular archivos o directorios en PHP, es necesario tener en cuenta los permisos de acceso y la carpeta de trabajo actual. Estos pueden afectar el resultado de las funciones mencionadas.

## Ver también

- Documentación oficial de PHP: https://www.php.net/manual/es/function.is-dir.php
- Ejemplos de uso de `is_dir()`: https://www.php.net/manual/es/function.is-dir.php#example-3816
- Más información sobre permisos de archivos y directorios: https://www.digitalocean.com/community/tutorials/como-configurar-permisos-de-acceso-y-propiedad-en-php-es