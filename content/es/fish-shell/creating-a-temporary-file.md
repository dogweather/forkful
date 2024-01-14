---
title:                "Fish Shell: Creando un archivo temporal"
simple_title:         "Creando un archivo temporal"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/fish-shell/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Por qué

Cuando estamos programando en Fish Shell, a menudo nos encontramos necesitando crear archivos temporales durante la ejecución del código. Estos archivos temporales pueden ser útiles para almacenar información temporalmente o para realizar operaciones específicas de forma más eficiente. En este artículo, aprenderemos cómo crear y utilizar archivos temporales en Fish Shell.

## Cómo hacerlo

Para crear un archivo temporal en Fish Shell, utilizaremos el comando `mktemp`. Este comando creará un archivo temporal con un nombre único y lo guardará en el directorio `/tmp`.

```Fish Shell
$ mktemp
/tmp/tmp.bUntE
```

También podemos especificar un prefijo para el nombre del archivo temporal utilizando la opción `-p`. Por ejemplo, si queremos que nuestro archivo temporal comience con la palabra "temp-", podemos usar el siguiente comando:

```Fish Shell
$ mktemp -p temp-
temp-Tyko3
```

Una vez que tenemos nuestro archivo temporal, podemos escribir en él utilizando el comando `echo`.

```Fish Shell
$ echo "¡Hola mundo!" >> temp-Tyko3
```

También podemos ver el contenido de nuestro archivo temporal utilizando el comando `cat`.

```Fish Shell
$ cat temp-Tyko3
¡Hola mundo!
```

Una vez que hayamos terminado de utilizar nuestro archivo temporal, debemos eliminarlo usando el comando `rm`.

```Fish Shell
$ rm temp-Tyko3
```

## Deep Dive

El comando `mktemp` no solo nos permite crear archivos temporales, sino que también nos brinda opciones para controlar mejor cómo se crean estos archivos. Por ejemplo, podemos especificar una extensión para el archivo utilizando la opción `-s`. También podemos elegir el directorio donde queremos que se guarde el archivo utilizando la opción `-d`.

Además, el comando `mktemp` nos ofrece la opción de crear un directorio temporal en lugar de un archivo, agregando la opción `-d`.

Otra opción útil del comando `mktemp` es la opción `-t`, que nos permite crear un archivo temporal dentro de un directorio previamente especificado. Esto puede ser útil si queremos tener un mayor control sobre dónde se guardan nuestros archivos temporales.

## Ver también

- [Documentación de Fish Shell](https://fishshell.com/docs/current/index.html)
- [Tutorial de Fish Shell](https://fishshell.com/docs/current/tutorial.html)
- [Comandos básicos de Fish Shell](https://fishshell.com/docs/current/commands.html)