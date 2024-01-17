---
title:                "Leyendo un archivo de texto"
html_title:           "C: Leyendo un archivo de texto"
simple_title:         "Leyendo un archivo de texto"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c/reading-a-text-file.md"
---

{{< edit_this_page >}}

## ¿Qué & Por qué?
Cuando un programador trabaja en un proyecto, a menudo necesita leer datos de un archivo de texto. Esto significa tomar la información almacenada en un archivo y utilizarla en el código del programa para realizar ciertas acciones. Los programadores hacen esto para poder utilizar datos almacenados externamente en sus programas y para evitar tener que ingresar manualmente grandes cantidades de datos.

## Cómo hacerlo:
Lo primero que debemos hacer es abrir el archivo de texto utilizando la función "fopen()". Luego, podemos utilizar la función "fgets()" para leer los datos del archivo línea por línea. Finalmente, cuando hayamos terminado de leer el archivo, debemos cerrarlo utilizando la función "fclose()". Aquí hay un ejemplo de cómo hacerlo en código:

```C
FILE *archivo;
char linea[100];

archivo = fopen("datos.txt", "r");
while(fgets(linea, 100, archivo) != NULL) {
  printf("%s", linea);
}
fclose(archivo);
```
El resultado de este código será imprimir cada línea del archivo "datos.txt". ¡Así de simple es leer un archivo de texto en un programa en C!

## Profundizando:
Históricamente, los programadores podían leer archivos utilizando la función "scanf()", pero esta no es una forma segura ya que puede causar problemas de seguridad. Otra alternativa es utilizar la función "fread()", pero esta es más compleja y no se recomienda para archivos de texto. La forma que hemos descrito arriba, utilizando "fopen()" y "fgets()", es la más segura y fácil de implementar para leer archivos de texto.

## Vea también:
Si quieres profundizar más sobre cómo trabajar con archivos en C, aquí hay algunos recursos adicionales que pueden ser útiles:
- [Documentación oficial de C sobre manejo de archivos](https://www.tutorialspoint.com/c_standard_library/c_function_fopen.htm)
- [Tutorial en vídeo sobre cómo leer y escribir archivos en C](https://www.youtube.com/watch?v=CBDBkLr8bVU)
- [Ejemplos de código para leer y escribir archivos en C](https://www.programiz.com/c-programming/examples/read-file)