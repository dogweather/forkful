---
title:    "C: Leyendo un archivo de texto"
keywords: ["C"]
---

{{< edit_this_page >}}

## ¿Por qué leer un archivo de texto en C?

Leer y manipular archivos de texto es una habilidad esencial en la programación en C. Muchas veces, necesitamos trabajar con grandes cantidades de datos almacenados en archivos de texto, y saber cómo leerlos nos permite realizar tareas útiles como análisis de datos, creación de informes y mucho más.

## Cómo hacerlo

Para leer un archivo de texto en C, primero debemos abrirlo utilizando la función `fopen()`. Esta función toma dos parámetros: el nombre del archivo y el modo de apertura. Por ejemplo, si queremos abrir un archivo llamado `datos.txt` en modo lectura, usaríamos `fopen("datos.txt", "r")`. Luego, podemos usar la función `fgets()` para leer línea por línea hasta que lleguemos al final del archivo.

```C
FILE *archivo;
char linea[100];

archivo = fopen("datos.txt", "r");

if (archivo == NULL) { // manejo de errores si el archivo no se puede abrir
  printf("Error al abrir el archivo.");
  exit(1);
}

while (fgets(linea, 100, archivo)) { // repetir hasta el final del archivo
  printf("%s", linea); // imprimir línea actual
}

fclose(archivo); // cerrar el archivo al finalizar
```

### Salida del ejemplo:

```
Este es un ejemplo de lectura de un archivo de texto en C.
La función `fgets()` nos permite leer línea por línea.
```

## Profundizando

Además de la función `fgets()`, también existen otras formas de leer archivos de texto en C, como `fgetc()`, `fread()` y `getchar()`. Cada una de estas funciones tiene sus propias ventajas y desventajas, y es importante conocerlas para decidir cuál es más adecuada para cada situación.

También es importante recordar cerrar el archivo después de terminar de leerlo, utilizando la función `fclose()`. De lo contrario, podemos correr el riesgo de perder datos.

## Vea también

- [Documentación oficial de C para la función `fopen()`](https://www.gnu.org/software/libc/manual/html_node/File-Open-Macro.html)
- [Artículo sobre lectura y escritura de archivos en C](https://www.tutorialspoint.com/cprogramming/c_file_io.htm)
- [Tutoriales de programación en C](https://www.programiz.com/c-programming)