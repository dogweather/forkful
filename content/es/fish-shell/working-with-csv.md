---
title:                "Trabajando con archivos csv"
html_title:           "Fish Shell: Trabajando con archivos csv"
simple_title:         "Trabajando con archivos csv"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/fish-shell/working-with-csv.md"
---

{{< edit_this_page >}}

## ¿Por qué trabajar con CSV?

Trabajar con archivos CSV (valores separados por comas) es una forma común de manejar datos en aplicaciones de programación. CSV es un formato sencillo y fácil de leer que se puede utilizar para almacenar grandes cantidades de información en una hoja de cálculo.

## Cómo hacerlo

Para trabajar con CSV en Fish Shell, es necesario utilizar el comando `csv` seguido de las opciones correspondientes. Por ejemplo, si queremos ver el contenido de un archivo CSV, podemos utilizar el siguiente comando:

```Fish Shell
csv read archivo.csv
```

Esto nos mostrará los datos del archivo CSV en la pantalla. También podemos utilizar el comando `csv write` para escribir datos a un archivo CSV.

Otra opción útil es el comando `csv query`, que nos permite realizar consultas a un archivo CSV utilizando comandos de SQL. Por ejemplo:

```Fish Shell
csv query "SELECT * FROM archivo.csv WHERE nombre = 'Juan'"
```

Este comando mostrará todas las filas del archivo CSV donde el nombre sea "Juan".

## Profundizando en el manejo de CSV

Fish Shell tiene varias funciones útiles para trabajar con archivos CSV. Por ejemplo, el comando `csv headers` nos mostrará los nombres de las columnas en un archivo CSV determinado. También podemos utilizar la opción `-d` para especificar un delimitador diferente al predeterminado (coma) en caso de que nuestro archivo CSV utilice un formato diferente.

Otra característica interesante de Fish Shell es que nos permite combinar datos de diferentes archivos CSV utilizando el comando `csv join`. Por ejemplo, podemos unir dos archivos CSV utilizando una columna en común como clave de unión.

¡No tengas miedo de experimentar con diferentes comandos y opciones para encontrar la mejor forma de trabajar con archivos CSV en Fish Shell!

## Ver también

- Documentación oficial de Fish Shell sobre CSV: https://fishshell.com/docs/current/cmds/csv.html
- Tutorial de CSV en Fish Shell: https://dev.to/aleksbash/working-with-csv-files-in-fish-shell-378n