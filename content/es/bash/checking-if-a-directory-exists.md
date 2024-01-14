---
title:                "Bash: Comprobando si existe un directorio"
simple_title:         "Comprobando si existe un directorio"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/bash/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Por qué

En programación, a menudo nos encontramos con la necesidad de comprobar si un directorio existe antes de realizar ciertas acciones en un script de Bash. Esto puede ser útil para evitar errores en el código o para asegurarnos de que el directorio necesario está presente antes de continuar con el flujo del programa. En esta entrada, vamos a explorar cómo comprobar si un directorio existe utilizando Bash.

## Cómo hacerlo

Existen varias formas de verificar si un directorio existe en Bash. Una de ellas es utilizar el comando `test` seguido de la opción `-d` para indicar que se quiere comprobar si un directorio existe y el nombre del directorio a comprobar. Por ejemplo:

```Bash
test -d directorio/
```

Esta línea de código devolverá un código de salida 0 si el directorio existe y un código de salida 1 si no existe.

Otra forma de hacerlo es utilizando el comando `cd` seguido del nombre del directorio. Si el directorio existe, el comando se ejecutará de manera satisfactoria y devolverá un código de salida 0, mientras que si el directorio no existe, se devolverá un código de salida distinto de 0. Por ejemplo:

```Bash
cd directorio/  # en caso de existir, este comando funcionará correctamente
```

También es posible utilizar el comando `ls` para mostrar el contenido de un directorio y luego redirigir la salida a `/dev/null`, que es una ubicación invisible del sistema de archivos donde se descartan todos los datos redirigidos hacia ella. Si el directorio no existe, se producirá un error y se mostrará un mensaje, mientras que si existe, no se producirá ningún error y el comando se ejecutará de manera correcta.

```Bash
ls directorio/ > /dev/null # esta línea de código no mostrará ningún error si el directorio existe
```

## Profundizando

Es importante tener en cuenta que el comando `test` y los códigos de salida mencionados anteriormente solo comprueban si el directorio existe o no, pero no dicen nada sobre los permisos de acceso al directorio. Si el usuario del script de Bash no tiene permisos suficientes para acceder al directorio, se obtendrá un código de salida distinto de 0 a pesar de que el directorio exista.

Además, también es posible utilizar una estructura de control condicional para realizar acciones diferentes según si el directorio existe o no. Por ejemplo:

```Bash
if [ -d directorio/ ]  # si el directorio existe
then
    echo "El directorio existe"
else
    echo "El directorio no existe"
fi
```

## Ver también

- [Cómo crear y eliminar directorios en Bash](https://www.ejemplo.com/crear-eliminar-directorios-bash)
- [Cómo copiar y mover archivos en Bash](https://www.ejemplo.com/copiar-mover-archivos-bash)
- [Documentación oficial de Bash](https://www.gnu.org/software/bash/)