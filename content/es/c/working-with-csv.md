---
title:                "Trabajando con archivos csv"
html_title:           "C: Trabajando con archivos csv"
simple_title:         "Trabajando con archivos csv"
programming_language: "C"
category:             "C"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c/working-with-csv.md"
---

{{< edit_this_page >}}

## Por qué

Si alguna vez has trabajado con hojas de cálculo o bases de datos, entonces probablemente estés familiarizado con el formato CSV. Este formato de archivo, que significa "valores separados por comas" en inglés, es una forma común de almacenar datos tabulares. En este artículo, veremos cómo trabajar con archivos CSV en el lenguaje de programación C y cómo puede ser útil para manipular grandes cantidades de datos.

## Cómo hacerlo

Para trabajar con archivos CSV en C, primero debemos incluir la biblioteca estándar `stdio.h` y la biblioteca `string.h`, que nos proporcionan funciones para manejar archivos y cadenas de caracteres.

```C
#include <stdio.h>
#include <string.h>
```

Una vez que tenemos estas bibliotecas incluidas, podemos usar la función `fopen()` para abrir un archivo CSV en modo de lectura o escritura:

```C
FILE *archivo = fopen("datos.csv", "r");
```

En este ejemplo, estamos abriendo un archivo llamado "datos.csv" en modo de solo lectura. Si desea abrir el archivo en modo de escritura, usaríamos `"w"` como segundo argumento de la función `fopen()`.

A continuación, utilizamos la función `fgetc()` para leer cada carácter del archivo CSV y almacenarlos en una variable de tipo `char`. Podemos usar un bucle while para leer el archivo hasta que alcancemos el final:

```C
int c;
while ((c = fgetc(archivo)) != EOF) {
    // hacer algo con cada carácter leído
}
```

Ahora que hemos leído el archivo, necesitamos almacenar los datos de alguna manera. Una forma común de hacerlo es utilizando un array multidimensional para almacenar cada fila y columna del CSV. Para hacer esto, primero deben determinar el tamaño del array. Podemos calcular automáticamente el número de filas y columnas con un bucle while contando el número de comas encontradas en cada fila.

```C
int filas = 0;
int columnas = 0;
c = 0;
while ((c = fgetc(archivo)) != EOF) {
    if (c == ',') {
        columnas++;
    }
    else if (c == '\n') {
        filas++;
    }
}
```

Una vez que conocemos el número de filas y columnas, podemos declarar el array multidimensional y utilizar la función `fscanf()` para leer cada elemento del CSV y almacenarlo en el array:

```C
float datos[filas][columnas];
rewind(archivo);

int fila, columna;
for (fila = 0; fila < filas; fila++) {
    for (columna = 0; columna < columnas; columna++) {
        fscanf(archivo, "%f,", &datos[fila][columna]);
    }
}
```

Finalmente, podemos imprimir los datos en pantalla o realizar cualquier operación que necesitemos con ellos.

## Profundizando

Si bien este es un ejemplo básico de cómo trabajar con archivos CSV en C, hay muchas formas de hacerlo y depende de cada situación particular. Por ejemplo, podemos utilizar la función `sscanf()` para leer una fila completa del CSV en una cadena de caracteres y luego dividir esa cadena en elementos individuales. También podemos utilizar la función `fprintf()` para escribir datos en un archivo CSV en lugar de leerlos.

Además, dependiendo de los datos que estemos tratando, es posible que necesitemos hacer algunas validaciones y conversiones de tipo antes de almacenarlos en el array. Por ejemplo, podemos usar la función `isalpha()` para verificar si un carácter es alfabético antes de convertirlo a un tipo numérico.

En resumen, trabajar con archivos CSV en C ofrece muchas posibilidades y puede ser una herramienta poderosa para manipular datos de manera eficiente.

## Ver También

- [Documentación oficial de la función `fopen()`](https://www.cplusplus.com/reference/cstdio/fopen/)
- [Cómo trabajar con cadenas de caracteres en C](https://www.geeksforgeeks.org/strings-in-c-2/)