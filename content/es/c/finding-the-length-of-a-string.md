---
title:                "C: Encontrar la longitud de una cadena"
programming_language: "C"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## ¡Por Qué!
En la programación, trabajar con cadenas de caracteres (strings) es una tarea común y esencial. En algunas situaciones, es necesario conocer la longitud de una cadena de caracteres para poder manipularla correctamente. En este artículo, aprenderemos cómo encontrar la longitud de una cadena en el lenguaje de programación C.

## Cómo hacerlo
Para encontrar la longitud de una cadena de caracteres en C, utilizaremos la función `strlen` de la biblioteca `string.h`. Esta función toma como argumento la cadena de caracteres y devuelve un valor de tipo `size_t` que representa su longitud. Veamos un ejemplo de cómo utilizarla:

```C
#include <stdio.h>
#include <string.h>

int main(){
  char nombre[] = "Juan";
  size_t longitud = strlen(nombre);
  printf("La longitud de la cadena '%s' es: %zu", nombre, longitud);
  return 0;
}
```

En este ejemplo, utilizamos la función `strlen()` para encontrar la longitud de la cadena `nombre` (que contiene "Juan") y la almacenamos en la variable `longitud`. Luego, imprimimos el resultado utilizando `printf`, donde `%zu` es el especificador de formato para un valor de tipo `size_t`. El resultado será: `La longitud de la cadena 'Juan' es: 4`.

## Profundizando
Ahora que sabemos cómo encontrar la longitud de una cadena en C, es importante conocer algunos detalles importantes sobre el tema. Por ejemplo, esta función cuenta todos los caracteres de la cadena, incluyendo el carácter nulo `'\0'`. Este es el carácter que se agrega automáticamente al final de una cadena en C y se utiliza para indicar el final de la misma.

También es importante mencionar que `strlen` es una función muy rápida, ya que solo recorre la cadena una vez. Esto hace que sea ideal para utilizar en programas que requieren de una ejecución eficiente.

## Ver También
- [Tutorial de C en Español](https://www.learn-c.org/es/)
- [Documentación oficial de la función `strlen()` en la biblioteca `string.h` en C](https://www.gnu.org/software/libc/manual/html_node/String-Length.html)
- [¿Por qué utilizar C en la programación?](https://medium.com/@arnaldorgil/por-qu%C3%A9-usar-c-en-2018-dc90de25fd25)