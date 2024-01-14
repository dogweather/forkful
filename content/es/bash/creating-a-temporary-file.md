---
title:    "Bash: Creando un archivo temporal"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/bash/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## ¿Por qué crear un archivo temporal?

Crear un archivo temporal puede ser útil en situaciones en las que se necesita almacenar datos temporales o ejecutar un comando que solo necesita ser ejecutado una vez.

## Cómo crear un archivo temporal

Crear un archivo temporal en Bash es fácil y se puede hacer utilizando el comando `mktemp`. Este comando creará automáticamente un archivo único y lo colocará en la ubicación especificada. Aquí hay un ejemplo:

```Bash
temp_file=$(mktemp)
echo "Este es un archivo temporal" > $temp_file
cat $temp_file
```

El primer comando `mktemp` asigna un archivo temporal único a la variable `temp_file`. Luego, usamos el comando `echo` para escribir una cadena en el archivo. Finalmente, usamos `cat` para imprimir el contenido del archivo temporal en la consola.

Si deseas crear un archivo temporal en una ubicación específica, puedes hacerlo pasando la ruta deseada como argumento al comando `mktemp`, como se muestra a continuación:

```Bash
temp_file=$(mktemp /ruta/al/archivo/temporal)
```

También puedes especificar un prefijo para el nombre del archivo temporal utilizando la opción `-t`, como se muestra en el siguiente ejemplo:

```Bash
temp_file=$(mktemp -t temp_file)
```

Debes tener en cuenta que el archivo temporal creado por `mktemp` se borra automáticamente una vez que el script o comando finaliza su ejecución. Si deseas retener el archivo temporal para su uso posterior, puedes utilizar la opción `-d` para crear un directorio temporal o la opción `-p` para especificar una ubicación específica y retener el archivo en esa ubicación.

## Inmersión profunda

El comando `mktemp` utiliza una plantilla para generar un nombre de archivo temporal único. Por defecto, esta plantilla es `tmp.XXXXXXXXXX`. Sin embargo, esta plantilla también se puede personalizar utilizando la opción `-u`.

Por ejemplo, si deseas que todos tus archivos temporales tengan un prefijo específico y un sufijo aleatorio, puedes especificar la plantilla de la siguiente manera:

```Bash
temp_file=$(mktemp -u /ruta/alArchivo/temporal/prefijo-XXXXXX-sufijo)
```

## Ver también

- [Cómo crear y usar archivos temporales en Bash](https://www.digitalocean.com/community/tutorials/how-to-create-and-use-temporary-files-in-bash-es)
- [Documentación oficial de `mktemp`](https://www.gnu.org/software/coreutils/manual/html_node/mktemp-invocation.html)