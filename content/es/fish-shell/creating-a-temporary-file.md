---
title:    "Fish Shell: Creando un archivo temporal"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

## ¿Por qué crear un archivo temporal en Fish Shell?

Crear un archivo temporal es una técnica común en la programación para almacenar datos temporales en el sistema. Esto es útil para procesar grandes cantidades de datos o para trabajar en scripts de forma eficiente.

## Cómo crear un archivo temporal en Fish Shell

En Fish Shell, hay varias maneras de crear un archivo temporal. Una forma sencilla es utilizar el comando `mktemp` seguido del prefijo `fish` para indicar que deseamos un archivo con extensión .fish. Por ejemplo:

```
fish mktemp temp.fish
```

Este comando generará un archivo temporal con el nombre `temp.fish` en el directorio actual. Podemos utilizar el comando `echo` para agregar contenido al archivo temporal, como se muestra a continuación:

```
echo "Hola, este es un archivo temporal" >> temp.fish
```

Si queremos crear un archivo temporal en un directorio específico, se puede utilizar la opción `-p` seguida del nombre del directorio. Por ejemplo:

```
fish mktemp -p ~/Documentos/temp temp.fish
```

Esto creará un archivo temporal llamado `temp.fish` en la carpeta `Documentos` con el prefijo `fish`.

## Profundizando en la creación de archivos temporales

Otra forma de crear archivos temporales en Fish Shell es utilizando la utilidad `touch`. Por ejemplo:

```
touch temp.fish
```

Esto creará un archivo vacío llamado `temp.fish` en el directorio actual. Podemos entonces utilizar el comando `echo` para agregar contenido al archivo temporal.

También podemos utilizar la variable de entorno `$TMPDIR` para crear un archivo temporal en el directorio de archivos temporales por defecto del sistema. Por ejemplo:

```
echo "Este es un archivo temporal" > $TMPDIR/temp.fish
```

En caso de que necesitemos un archivo temporal con una extensión diferente a `.fish`, podemos utilizar el comando `mktemp -u` seguido del sufijo que deseamos. Por ejemplo:

```
fish mktemp -u .txt
```

Esto creará un nombre de archivo único con la extensión .txt, que podemos utilizar para crear un archivo temporal.

## Ver también
- [Comandos básicos de Fish Shell](https://fishshell.com/docs/current/tutorial.html)
- [Documentación de Fish Shell sobre archivos temporales](https://fishshell.com/docs/current/index.html#redirects-temporary-files)
- [Cómo utilizar variables de entorno en Fish Shell](https://fishshell.com/docs/current/variables.html)
- [Tutorial de MKTEMP en Linux](https://www.geeksforgeeks.org/mktemp-command-in-linux-with-examples/)