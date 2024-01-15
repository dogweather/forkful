---
title:                "Escribir a error estándar"
html_title:           "C: Escribir a error estándar"
simple_title:         "Escribir a error estándar"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Por qué

Es importante aprender a escribir en el estándar de error en C porque es una forma útil de depurar o solucionar problemas en tus programas. Al imprimir mensajes de error en el estándar de error, puedes identificar fácilmente dónde se producen errores en tu código y corregirlos eficientemente.

## Cómo hacerlo

Escribir en el estándar de error en C es muy simple. Todo lo que necesitas es utilizar la función `fprintf()` y especificar `stderr` como el primer argumento. Aquí hay un ejemplo de cómo imprimir un mensaje de error en el estándar de error:

```C
fprintf(stderr, "Error: No se pudo abrir el archivo.\n");
```

El `\n` al final del mensaje es un carácter de nueva línea que nos asegura que el mensaje se imprima en una línea separada. Puedes utilizar cualquier otro carácter de escape como `\t` para crear una tabulación en el mensaje.

Ahora, si ejecutas este código y hay un error al abrir el archivo, verás el mensaje impreso en el estándar de error de esta manera:

```
Error: No se pudo abrir el archivo.
```

## Profundizando

El estándar de error (`stderr`) es una salida de error predeterminada en la mayoría de los sistemas operativos. Si seleccionas la salida de error en lugar de la salida estándar (`stdout`), tus mensajes de error se imprimirán en un lugar diferente al de la salida de tu programa. Esto es útil, ya que puedes redirigir fácilmente la salida estándar a un archivo mientras mantienes tus mensajes de error en la pantalla.

Otra función útil para escribir en el estándar de error es `perror()`, que toma un argumento de cadena y automáticamente agrega un mensaje de error asociado al valor actual de `errno`. Aquí hay un ejemplo:

```C
FILE* archivo = fopen("archivo.txt", "r");
if (archivo == NULL) {
    perror("Error");
    exit(EXIT_FAILURE);
}
```

Si ocurre un error al abrir el archivo, se imprimirá un mensaje como este:

```
Error: No existe el archivo o directorio
```

## Otras consideraciones

Recuerda que el estándar de error es una salida no almacenada, lo que significa que no la puedes guardar en una variable. Si necesitas almacenar un mensaje de error para su uso posterior, puedes redirigir la salida estándar a un archivo o utilizar un buffer para guardar el mensaje.

## Ver también

- [La función fprintf en C](https://www.geeksforgeeks.org/fprintf-in-c/)
- [La función perror en C](https://www.geeksforgeeks.org/perror-in-c/)