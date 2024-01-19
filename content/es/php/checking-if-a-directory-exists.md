---
title:                "Verificando si un directorio existe"
html_title:           "PHP: Verificando si un directorio existe"
simple_title:         "Verificando si un directorio existe"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/php/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## ¿Qué es y por qué?

Verificar si un directorio existe es una operación común en PHP que nos ayuda a determinar si una ruta de carpeta específica ya está en uso o no. Los programadores lo hacen para evitar errores al intentar crear un directorio que ya existe o acceder a uno que no existe.

## Cómo hacerlo:

La función incorporada `is_dir()` en PHP puede ser usada para chequear si un directorio existe. Aquí te muestro cómo:

```PHP
<?php
$directorio = 'ruta/del/directorio';

if(is_dir($directorio)){
    echo 'El directorio existe.';
} else {
    echo 'El directorio no existe.';
}
?>
```

El código anterior lanzará 'El directorio existe.' si la ruta del directorio especificada existe. De lo contrario, imprimirá 'El directorio no existe.'.

## Inmersión profunda:

### Contexto histórico
La función `is_dir()` ha estado en PHP desde su versión 4. La necesidad de verificar la existencia de un directorio es tan antigua como el lenguaje en sí.

### Alternativas
Una alternativa a `is_dir()` es usar `file_exists()`, pero esto también verificará si un archivo existe. Por lo tanto, `is_dir()` es más específico para directorios.

```PHP
<?php
$directorio = 'ruta/del/directorio';

if(file_exists($directorio)){
    echo 'El directorio existe.';
} else {
    echo 'El directorio no existe.';
}
?>
```

### Implementación
Internamente, `is_dir()` hace una llamada al sistema operativo para verificar si el directorio existe. Esto significa que el rendimiento de `is_dir()` puede variar dependiendo del sistema operativo y del sistema de archivos.

## Ver también:

1. Función is_dir() en PHP - [Manual de PHP](https://www.php.net/manual/es/function.is-dir.php)
2. Función file_exists() en PHP - [Manual de PHP](https://www.php.net/manual/es/function.file-exists.php)