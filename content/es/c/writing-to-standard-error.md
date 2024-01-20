---
title:                "Escribir en el error estándar"
html_title:           "C: Escribir en el error estándar"
simple_title:         "Escribir en el error estándar"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

Escribir a la salida estándar de error es una técnica común en programación en C. Consiste en imprimir mensajes de error o advertencias en lugar de la salida estándar, lo que permite al programador identificar y corregir errores en su código de manera más eficiente.

## Cómo:

Vamos a ver un ejemplo simple de cómo escribir a la salida estándar de error en C:

```
#include <stdio.h>

int main() {
  fprintf(stderr, "¡Hola, Mundo!\n");
  return 0;
}
```

Este código imprime "¡Hola, Mundo!" en la salida estándar de error, en lugar de en la salida estándar normal.

Al ejecutar este programa, verás el siguiente resultado:

```
¡Hola, Mundo!
```

## Profundizando:

Escribir a la salida estándar de error tiene una larga historia en la programación en C. Anteriormente, era común que los programadores escribieran mensajes de error a un archivo, lo que requería más código y recursos. Por lo tanto, escribir a la salida estándar de error se convirtió en una alternativa más eficiente y práctica.

En lugar de utilizar `fprintf` como en el ejemplo anterior, también puedes utilizar la función `perror` para imprimir mensajes de error predefinidos, como en el siguiente ejemplo:

```
#include <stdio.h>

int main() {
  FILE *fp = fopen("archivo_que_no_existe.txt", "r");
  if (fp == NULL) {
    perror("Error al abrir el archivo:");
  }
  return 0;
}
```

Este código imprime el mensaje de error "Error al abrir el archivo: No such file or directory" en la salida estándar de error.

## Ver también:

- [Escribir a la salida estándar de error en otros lenguajes de programación](https://es.stackoverflow.com/questions/165/por-qu%C3%A9-escribir-en-la-salida-de-error-stderr-rather-than-stdout)