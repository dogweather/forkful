---
title:                "Trabajando con archivos csv"
html_title:           "Bash: Trabajando con archivos csv"
simple_title:         "Trabajando con archivos csv"
programming_language: "Bash"
category:             "Bash"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/bash/working-with-csv.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué? 
Trabajar con CSV (Comma-Separated Values) significa manejar archivos de texto estructurados en columnas y filas, que pueden ser exportados y leídos por diferentes programas. Los programadores usan CSV porque es una forma sencilla de almacenar y compartir datos de manera eficiente. 

## ¡Cómo hacerlo!
Aquí te mostramos cómo importar un archivo CSV a una tabla de MySQL utilizando la opción LOAD DATA INFILE. Primero, asegúrate de tener habilitada la opción secure_file_priv para que MySQL pueda acceder a los archivos CSV en tu sistema. Luego, ejecuta este comando en tu consola Bash:

```
mysql -e "LOAD DATA INFILE 'ruta/al/archivo.csv' 
        INTO TABLE nombre_de_tabla 
        FIELDS TERMINATED BY ',' 
        ENCLOSED BY '\"' 
        LINES TERMINATED BY '\n' 
        IGNORE 1 LINES;"
```

El resultado será la importación exitosa de tus datos del archivo CSV a la tabla especificada en MySQL.

## Deep Dive
El formato CSV fue desarrollado en los años 70 como una forma sencilla de almacenar y compartir datos en distintos sistemas. Aunque es ampliamente utilizado para almacenar datos tabulares, existen otros formatos como JSON y XML que también son comunes en el mundo de la programación. En Bash, también es posible trabajar con estos formatos utilizando herramientas como jq y xmllint.

## Ver también
Para obtener más información sobre el formato CSV y cómo trabajar con él en Bash, puedes consultar la documentación oficial de MySQL: [MySQL Documentation](https://dev.mysql.com/doc/refman/8.0/en/load-data.html) y la página de Bash en la Wiki de Ubuntu: [Bash Documentation](https://wiki.ubuntu.com/Documentation/Howto/Manipulating_CSV_files).