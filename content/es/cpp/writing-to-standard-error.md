---
title:                "C++: Escribiendo en el error estándar"
simple_title:         "Escribiendo en el error estándar"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/cpp/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## ¿Por qué escribir en standard error?

Escribir en standard error es una técnica comúnmente utilizada en programación para imprimir mensajes de error o de depuración en la pantalla o en un archivo de registro. Esto permite a los desarrolladores identificar y solucionar problemas en su código más rápidamente, lo que resulta en un proceso de desarrollo más eficiente y efectivo.

En esta publicación, aprenderemos cómo usar la función estándar `fprintf()` en C++ para escribir en standard error y profundizaremos en su funcionamiento.

## Cómo hacerlo

Para escribir en standard error en C++, usaremos la función `fprintf()`, que pertenece a la biblioteca `cstdio`. Aquí hay un ejemplo de cómo usarla en un código simple:

```C++
#include <cstdio>

int main() {
    fprintf(stderr, "El valor de i es %d", i); // Escribe en standard error
    // Resto del código
    return 0;
}
```

En el ejemplo anterior, usamos la función `fprintf()` para escribir un mensaje de error en standard error, utilizando el especificador de formato `%d` para imprimir el valor de la variable `i`. Es importante tener en cuenta que usamos `fprintf()` en lugar de `printf()` para que el mensaje se imprima en standard error en lugar de standard output.

## Profundizando

Cuando escribimos en standard error, estamos enviando nuestros mensajes a uno de los tres dispositivos disponibles: standard output, standard error o un archivo de registro. A veces, es posible que queramos redirigir nuestros mensajes de error a un archivo de registro en lugar de imprimirlos en la pantalla. Para hacerlo, podemos usar la utilidad de línea de comandos `2>` en Linux o la función `freopen()` en C++ para redirigir el flujo de salida.

También podemos cambiar el color del texto impreso en standard error para hacer que los mensajes sean más visibles y llamativos. Esto se puede hacer usando códigos ANSI de escape en la función `fprintf()`. Por ejemplo, para imprimir un mensaje de error en rojo, podemos usar `fprintf(stderr, "\x1B[31mError!");`.

En resumen, escribir en standard error es una técnica útil para mejorar nuestro proceso de desarrollo al permitirnos identificar y solucionar problemas en nuestro código de manera más rápida y eficiente. Además, podemos redirigir nuestros mensajes de error a diferentes dispositivos o cambiar su apariencia para hacerlos más notorios.

## Ver también

- [Documentación de la función `fprintf()` en cppreference.com](https://en.cppreference.com/w/c/io/fprintf)
- [Tutorial de redirección de streams en Linux](https://www.digitalocean.com/community/tutorials/an-introduction-to-linux-i-o-redirection)
- [Guía de códigos ANSI de escape en Wikipedia](https://en.wikipedia.org/wiki/ANSI_escape_code)