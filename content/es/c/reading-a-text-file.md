---
title:                "Leyendo un archivo de texto"
html_title:           "Arduino: Leyendo un archivo de texto"
simple_title:         "Leyendo un archivo de texto"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c/reading-a-text-file.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?

Leer un archivo de texto en programación se refiere a la extracción de datos almacenados en un archivo de texto de formato simple. Los programadores lo hacen para manipular datos guardados o almacenados previamente, generalmente para análisis o procesamiento.

## Cómo hacer:

A continuación, se muestra un código de ejemplo que lee un archivo de texto en C:

```C
#include <stdio.h>

int main() {
    FILE *archivo;
    char caracter;

    archivo = fopen("archivo.txt","r");

    if(archivo == NULL) {
        printf("\nEl archivo no se puede abrir. \n");
    } else {
        printf("\nEl contenido del archivo es: \n");

        while((caracter = fgetc(archivo)) != EOF) {
	    printf("%c",caracter);
	}
    }

    fclose(archivo);

    return 0;
}
```
Salida de muestra del código anterior:

```C
El contenido del archivo es:
Hola Mundo
```

## Inmersión Profunda:

Históricamente, la capacidad de leer un archivo de texto es antigua en la programación, pero sigue siendo esencial. Tiene usos en muchas áreas, desde programación de sistemas básicos hasta data science y machine learning.

Hay alternativas a la lectura de archivos de texto en C. Por ejemplo, se puede usar fread() en lugar de fgetc(). Sin embargo, fgetc() es más simple y mejor para los principiantes.

Detalles de implementación: La función fopen() se utiliza para abrir una corriente de archivo. fgetc() se utiliza para obtener el siguiente carácter de la corriente y EOF (End of File)[Fin de Archivo] es un indicador para terminar la lectura cuando se alcanza el final del archivo.

## Ver También:

- Más sobre fgetc(): https://devdocs.io/c/io/fgetc
- Alternativas para leer archivos: https://www.geeksforgeeks.org/c-programming-file-io-character-wise/  
- Documentación general de C: https://www.gnu.org/software/gnu-c-manual/