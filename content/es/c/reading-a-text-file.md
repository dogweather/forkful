---
title:                "C: Leyendo un archivo de texto"
simple_title:         "Leyendo un archivo de texto"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Por qué

¿Alguna vez te has preguntado cómo los programas en lenguaje C son capaces de leer archivos de texto? ¿Qué sucede detrás de escena para que puedas abrir un archivo y ver su contenido en la pantalla? Si quieres conocer los detalles detrás de esta funcionalidad, ¡sigue leyendo!

## Cómo hacerlo

Para leer un archivo de texto en lenguaje C, primero debemos abrirlo. Esto se hace utilizando la función `fopen()` que acepta dos argumentos: el nombre del archivo y el modo de apertura. Por ejemplo, si queremos abrir un archivo llamado "texto.txt" en modo de lectura, podemos utilizar la siguiente línea de código:

```C
FILE *fp = fopen("texto.txt", "r");
```

Una vez que tenemos el archivo abierto, podemos utilizar la función `fscanf()` para leer el contenido de este. Esta función funciona de manera similar a `scanf()` pero en lugar de leer desde la entrada estándar (como el teclado), lee desde el archivo. Por ejemplo, si queremos imprimir en pantalla el contenido de un archivo línea por línea, podemos hacer lo siguiente:

```C
char linea[100];
while (fscanf(fp, "%s", linea) != EOF) {
  printf("%s\n", linea);
}
```

Este código funciona leyendo cada línea del archivo y guardándola en un arreglo `linea` de tamaño 100. Luego, imprimimos esa línea en pantalla y seguimos leyendo hasta llegar al final del archivo (EOF).

## Profundizando

Ahora que sabemos cómo abrir y leer un archivo de texto en lenguaje C, podemos profundizar un poco más y hablar sobre la función `fscanf()`. Esta función acepta un formato como argumento, que le indica cómo debe interpretar los datos que está leyendo. Por ejemplo, si nuestro archivo contiene una línea con dos números separados por un espacio, podemos utilizar la siguiente línea de código para leer esos dos números y almacenarlos en dos variables diferentes:

```C
fscanf(fp, "%d %d", &num1, &num2);
```

En este caso, utilizamos el formato `%d` para indicar que estamos leyendo un número entero. También podemos utilizar otros formatos, como `%f` para números flotantes o `%s` para cadenas de caracteres.

## Ver también

Ahora que sabes cómo leer un archivo de texto en lenguaje C, puedes profundizar en otras funcionalidades relacionadas, como escribir en un archivo o manejar errores al abrirlo. Aquí tienes algunos enlaces útiles para continuar aprendiendo:

- [Cómo escribir en un archivo en lenguaje C](https://medium.com/@wesbragagt/how-to-write-to-a-file-in-c-5a6ff4dca5e7)
- [Manejo de errores al abrir archivos en lenguaje C](https://www.tutorialspoint.com/cprogramming/c_error_handling.htm)
- [Documentación de la función fscanf()](https://www.tutorialspoint.com/c_standard_library/c_function_fscanf.htm)