---
title:    "Bash: Comprobando si existe un directorio"
keywords: ["Bash"]
---

{{< edit_this_page >}}

## Por qué

A menudo, cuando estamos programando en Bash, necesitamos saber si un directorio existe o no. Esto puede ser útil al crear scripts para automatizar tareas o para realizar comprobaciones de errores en nuestro código.

## Cómo

Para verificar si un directorio existe en Bash, utilizamos el comando `test`, seguido de la condición `-d` que indica que estamos buscando un directorio, y por último, especificamos la ruta del directorio que queremos comprobar. Por ejemplo:

```Bash
test -d /home/usuario/Escritorio
```

Si el directorio existe, no se mostrará ninguna salida en la terminal. En cambio, si no existe, recibiremos un mensaje de error indicando que no se pudo encontrar el directorio.

Otra forma de verificar si un directorio existe es utilizando el comando `if`, que nos permite ejecutar una acción si la condición es verdadera y otra acción si es falsa. En este caso, utilizaremos la variable `$?`, que almacena el resultado de la última ejecución exitosa. Si es igual a 0, significa que la condición fue verdadera y, por lo tanto, el directorio existe. Por ejemplo:

```Bash
if [ $? -eq 0 ]
then 
  echo "El directorio existe"
else
  echo "El directorio no existe"
fi
```

Para obtener una lista de todos los directorios en una ruta determinada, podemos utilizar el comando `ls` y la opción `-d`, que nos devuelve solo los nombres de los directorios sin mostrar su contenido. Por ejemplo:

```Bash
ls -d /home/usuario/*
```

## Profundizando

Además de verificar si un directorio existe en una ruta específica, también podemos utilizar la comparación lógica `&&` para realizar una acción si la condición es verdadera y utilizar la comparación lógica `||` para realizar una acción si la condición es falsa.

Por ejemplo, podemos utilizar `&&` para crear un directorio si este no existe previamente. Si el directorio ya existe, no se ejecutará la acción de crearlo. Además, podemos utilizar `||` para mostrar un mensaje de error si el directorio no existe y, al mismo tiempo, crearlo. Por ejemplo:

```Bash
test -d /home/usuario/Escritorio && echo "El directorio ya existe" || mkdir /home/usuario/Escritorio && echo "Se ha creado el directorio"
```

## Ver también

- [Bash Guide for Beginners (en inglés)](https://linuxconfig.org/bash-scripting-tutorial-for-beginners)
- [Shell Scripting Tutorial (en inglés)](https://www.shellscript.sh/index.html)
- [Documentación oficial de Bash (en inglés)](https://www.gnu.org/software/bash/manual/bash.html)