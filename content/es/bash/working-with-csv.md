---
title:                "Bash: Trabajando con csv"
simple_title:         "Trabajando con csv"
programming_language: "Bash"
category:             "Bash"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/bash/working-with-csv.md"
---

{{< edit_this_page >}}

## ¿Por qué trabajar con CSV?
CSV (Comma-Separated Values) es uno de los formatos más populares para almacenar datos en una tabla. Es utilizado por muchas aplicaciones y sistemas para importar y exportar información, por lo que tener conocimientos en cómo trabajar con este formato puede ser muy útil para diversas tareas. En este artículo aprenderemos a manejar archivos CSV utilizando Bash.

## Cómo hacerlo
Lo primero que necesitamos para trabajar con CSV en Bash es tener un archivo en este formato. Podemos crear uno manualmente, o utilizar uno existente como ejemplo. Una vez tengamos el archivo, podemos utilizar comandos de Bash para manipularlo y extraer la información que necesitemos.

Veamos un ejemplo de cómo podemos leer un archivo CSV y mostrar su contenido en la terminal:

```Bash
#!/bin/bash

while IFS=, read -r col1 col2 col3
do
  echo "Columna 1: $col1, Columna 2: $col2, Columna 3: $col3"
done < archivo.csv
```

En este ejemplo, se utiliza el comando `read` para tomar cada columna del archivo y asignarla a una variable. Luego, se utiliza un bucle `while` para leer cada línea del archivo, separando las columnas por medio de la coma. Finalmente, se muestra la información en la terminal.

Podemos también utilizar el comando `awk` para filtrar y manipular la información de nuestro archivo CSV. Por ejemplo, si queremos mostrar solo las filas que contengan cierto valor en una columna en específico, podemos utilizar el siguiente comando:

```Bash
awk -F "," '$2 == "Valor" {print $0}' archivo.csv
```

Este comando especifica que el delimitador del archivo es la coma (`,`) y luego filtra solo las filas donde la segunda columna tenga el valor especificado. Luego, se imprime la fila completa utilizando el parámetro `$0`.

## Profundizando
Existen muchas otras herramientas y comandos disponibles en Bash para trabajar con CSV. Podemos utilizar `sed` para realizar cambios en el formato del archivo, `grep` para buscar información específica, `sort` para ordenar las filas y mucho más. También podemos combinar estos comandos para realizar tareas más complejas.

Es importante tener en cuenta que Bash es un lenguaje muy versátil, por lo que las posibilidades son infinitas y dependerán de las necesidades de cada proyecto.

## Consulta también
- [Documentación oficial de Bash](https://www.gnu.org/software/bash/)
- [Tutorial sobre cómo trabajar con archivos CSV en Bash](https://linuxize.com/post/bash-read-csv-file/)
- [Más ejemplos de utilización de comandos Bash para manipular CSV](https://www.cyberciti.biz/faq/unix-linux-bash-read-comma-separated-csvfile/)
- [Ejemplos interactivos para practicar comandos de Bash](https://www.tutorialspoint.com/unix_terminal_online.php)