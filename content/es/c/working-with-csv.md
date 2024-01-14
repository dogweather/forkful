---
title:                "C: Trabajando con archivos csv"
simple_title:         "Trabajando con archivos csv"
programming_language: "C"
category:             "C"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c/working-with-csv.md"
---

{{< edit_this_page >}}

# ¿Por qué trabajar con archivos CSV en programación C?

CSV (Comma Separated Values) son archivos de texto utilizados para almacenar y organizar datos en forma de tabla, donde cada valor se separa por una coma. Son ampliamente utilizados en la programación C debido a su simplicidad y compatibilidad con una gran variedad de aplicaciones. Además, trabajar con archivos CSV en C puede facilitar el manejo de grandes cantidades de datos de manera eficiente.

## Cómo utilizar archivos CSV en programación C

Para trabajar con archivos CSV en C, primero debemos incluir la librería `stdio.h` en nuestro código. A continuación, utilizaremos la función `fopen` para abrir el archivo CSV y la función `fprintf` para escribir datos en el archivo. Por ejemplo:

```C
#include <stdio.h>

int main() {
    FILE *fp;
    fp = fopen("datos.csv", "w"); // Abre un archivo llamado "datos.csv" para escribir
    fprintf(fp,"Nombre,Edad\n"); // Escribe los nombres de las columnas
    fprintf(fp,"Juan,25\n"); // Escribe los datos de la primera fila
    fprintf(fp,"María,30\n"); // Escribe los datos de la segunda fila
    fclose(fp); // Cierra el archivo
    return 0;
}
```

El resultado de este código será un archivo CSV con dos columnas (Nombre y Edad) y dos filas con los datos proporcionados.

## Profundizando en el trabajo con archivos CSV en C

Además de escribir datos en archivos CSV, también es posible leer datos de un archivo existente. Para ello, utilizamos la función `fscanf` en lugar de `fprintf`. También podemos utilizar la función `fgetcsv` para leer datos de un archivo específico en formato CSV.

Es importante tener en cuenta que, al trabajar con archivos CSV en C, es necesario manejar adecuadamente los errores que puedan surgir durante la lectura o escritura de datos. Para ello, se recomienda utilizar la función `perror` para mostrar un mensaje de error en caso de que algo salga mal.

## Ver también

- Documentación oficial de la librería `stdio.h` (https://www.cplusplus.com/reference/cstdio/)
- Tutorial sobre el manejo de archivos CSV en C (https://www.tutorialspoint.com/c_standard_library/c_function_fscanf.htm)
- Ejemplos de código para trabajar con archivos CSV en C (https://www.geeksforgeeks.org/csv-file-management-using-c/)
- Más información sobre el formato CSV (https://www.csvfile.com/csv-file-example/)