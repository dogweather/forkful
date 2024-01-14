---
title:    "Bash: Creando un archivo temporal"
keywords: ["Bash"]
---

{{< edit_this_page >}}

## Por qué

Crear un archivo temporal es una técnica útil en la programación de Bash. Estos archivos son utilizados para almacenar datos temporales que se generan durante la ejecución del programa y que no necesitan ser guardados permanentemente. Además, también se pueden utilizar para realizar pruebas de código sin afectar a archivos permanentes importantes.

## Cómo

Para crear un archivo temporal en Bash, podemos utilizar el comando `mktemp`. Este comando generará un nombre único para el archivo temporal y lo creará en el directorio actual. Por ejemplo:

```Bash
temp_file=$(mktemp)
echo "Este es un archivo temporal creado con Bash" > $temp_file
cat $temp_file
```

La salida de este código será: `Este es un archivo temporal creado con Bash`.

Si deseamos especificar un prefijo para el nombre del archivo temporal, podemos utilizar el siguiente comando:

```Bash
temp_file=$(mktemp mi_archivo_temporal.XXXXXX)
```

El parámetro `XXXXXX` se reemplazará con un código aleatorio, lo que garantiza que el nombre del archivo será único. También podemos utilizar una plantilla de nombre para el archivo temporal, que se indicará con el sufijo `.XXXXXXXXXX` en la plantilla del nombre. Por ejemplo:

```Bash
temp_file=$(mktemp mi_archivo_temporal.XXXXXXXXXX)
```

En cuanto al formato del nombre del archivo temporal, podemos especificar una extensión utilizando el mismo comando. Por ejemplo:

```Bash
temp_file=$(mktemp mi_archivo_temporal_XXXXXX.txt)
```

En este caso, el archivo temporal se creará con la extensión `.txt`.

## Profundizando

Además del comando `mktemp`, también podemos utilizar otras herramientas para crear archivos temporales en Bash, como el comando `touch` o el redireccionamiento de salida de un comando a un archivo. Sin embargo, el uso del comando `mktemp` nos garantiza que el archivo creado será único y se eliminará automáticamente al finalizar la ejecución del programa, evitando posibles conflictos con archivos permanentes.

Otra técnica útil es utilizar la variable de entorno `TMPDIR` para especificar el directorio en el que deseamos crear el archivo temporal. Por defecto, el comando `mktemp` utilizará el directorio `/tmp`, pero podemos cambiarlo según nuestras necesidades.

## Ver también

- [Comandos básicos de Bash](https://www.hostinger.es/tutoriales/comandos-basicos-de-bash/)
- [Crear y eliminar archivos temporales en Bash](https://www.enmimaquinafunciona.com/pregunta/97346/como-crear-y-borrar-un-archivo-temporal-en-bash)
- [Documentación oficial de Bash](https://www.gnu.org/software/bash/)