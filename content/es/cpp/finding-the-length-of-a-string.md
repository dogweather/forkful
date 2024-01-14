---
title:                "C++: Encontrar la longitud de una cadena"
simple_title:         "Encontrar la longitud de una cadena"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/cpp/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Por qué

¿Alguna vez te has preguntado cómo los programas pueden determinar la longitud de una cadena de texto? Aunque parezca una pregunta sencilla, la respuesta puede ser bastante complicada. En esta publicación, veremos por qué es importante saber cómo encontrar la longitud de una cadena en un programa en C ++.

## Cómo hacerlo

Para encontrar la longitud de una cadena en C ++, podemos utilizar la función `strlen()`. Esta función se encuentra en la biblioteca de C string y devuelve un valor entero que representa la longitud de la cadena. Veamos un ejemplo de cómo podríamos utilizar esta función en nuestro programa:

```C++
#include <iostream>
#include <cstring>

using namespace std;

int main() {
    char cadena[] = "Hola mundo";
    int longitud = strlen(cadena);
    cout << "La longitud de la cadena es: " << longitud << endl;
    
    return 0;
}
```

El código anterior imprimirá en pantalla: `La longitud de la cadena es: 10`, ya que la cadena "Hola mundo" tiene una longitud de 10 caracteres.

Otra forma de encontrar la longitud de una cadena es utilizando un bucle `while`. Este bucle recorrerá la cadena hasta encontrar el carácter nulo `'\0'`, que indica el final de una cadena en C ++. Veamos un ejemplo de cómo implementar esto:

```C++
#include <iostream>

using namespace std;

int main() {
    char cadena[] = "¡Hola!";
    int longitud = 0;
    
    while (cadena[longitud] != '\0') {
        longitud++;
    }
    
    cout << "La longitud de la cadena es: " << longitud << endl;
    
    return 0;
}
```

Este código también imprimirá en pantalla `La longitud de la cadena es: 5`.

## Profundizando

Ahora que sabemos cómo encontrar la longitud de una cadena en C ++, veamos un poco más sobre cómo funciona esto en el fondo. La función `strlen()` utiliza un puntero a la primera posición de la cadena y recorre cada uno de los caracteres hasta encontrar el carácter nulo `'\0'`. Mientras tanto, la variable `longitud` se va incrementando en cada iteración del bucle.

Es importante mencionar que la longitud de la cadena no incluye el carácter nulo `'\0'`, por lo que siempre debemos asegurarnos de que nuestro bucle `while` o llamada a `strlen()` no lo tenga en cuenta para obtener un resultado preciso.

## Ver también

- [std::strlen - Referencia de C++](https://es.cppreference.com/w/cpp/string/byte/strlen)
- [Introducción a las cadenas en C++](https://openwebinars.net/blog/cadenas-de-caracteres-en-c/)