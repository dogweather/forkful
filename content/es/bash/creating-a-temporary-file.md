---
title:                "Creando un archivo temporal"
html_title:           "Bash: Creando un archivo temporal"
simple_title:         "Creando un archivo temporal"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/bash/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## ¿Por qué crear un archivo temporal en Bash?

Existen varias razones por las cuales uno puede necesitar crear un archivo temporal en un script de Bash. Por ejemplo, puede ser útil cuando se necesita almacenar temporalmente datos o resultados intermedios en un proceso de script. También puede ser útil para realizar pruebas en un entorno aislado sin afectar archivos existentes.

## Cómo hacerlo

Crear un archivo temporal en Bash es muy sencillo y se puede hacer de varias maneras. A continuación se muestran dos ejemplos de código que pueden ser útiles en diferentes situaciones:

#### Ejemplo 1:
```Bash
# Crea un archivo temporal con el número de proceso del script como parte del nombre
temp_file="/tmp/script_temp_$$.txt"

# Escribe datos en el archivo temporal
echo "Esto es un archivo temporal" > "$temp_file"

# Lee datos del archivo temporal
cat "$temp_file"

# Elimina el archivo temporal
rm "$temp_file"
```

#### Ejemplo 2:
```Bash
# Crea un archivo temporal con una fecha y hora como parte del nombre
temp_file="/tmp/$(date +%Y%m%d%H%M%S)_temp.txt"

# Escribe datos en el archivo temporal
echo "Esto es otro archivo temporal" > "$temp_file"

# Lee datos del archivo temporal
cat "$temp_file"

# Elimina el archivo temporal
rm "$temp_file"
```

En ambos ejemplos, se utiliza la variable `$temp_file` para almacenar la ruta del archivo temporal. Luego, se puede escribir o leer datos en ese archivo utilizando comandos de Bash como `echo` o `cat`. Por último, se elimina el archivo utilizando el comando `rm` una vez que ya no es necesario.

## Profundizando

Ahora, veamos cómo funciona la primera línea de cada ejemplo para crear el nombre del archivo temporal. En ambos casos, se utiliza una sintaxis similar, la cual puede ser desglosada de la siguiente manera:

- `/tmp/` es el directorio donde se almacenará el archivo temporal. Este directorio es comúnmente utilizado para almacenar archivos temporales en sistemas Unix.
- `$()` es una forma de ejecutar un comando dentro de un comando en Bash. En este caso, se utiliza para ejecutar el comando `date` y obtener la fecha y hora actual.
- `$$` es una variable especial que contiene el número de proceso del script que se está ejecutando.
- `_%Y%m%d%H%M%S` es una cadena de formato que se utiliza para obtener la fecha y hora actual en el formato deseado. En este caso, `%Y` representa el año, `%m` el mes, `%d` el día, `%H` la hora en formato de 24 horas, `%M` los minutos y `%S` los segundos.

Al concatenar todas estas partes, se obtiene una ruta única para el archivo temporal, que evitará conflictos con otros archivos temporales que puedan existir en el mismo directorio. Es importante notar que no se utiliza la extensión del archivo en la creación del nombre, ya que esto puede variar dependiendo del tipo de archivo que se vaya a almacenar.

## Ver también

- [Explicación detallada de cómo crear archivos temporales en Bash](https://www.baeldung.com/linux/create-temporary-files-bash)
- [Otras formas de generar nombres únicos para archivos temporales en Bash](https://www.johneday.com/422/time-in-unix-shell-script-execution-time-script-lesson-learning)