---
title:                "C++: Encontrando la longitud de una cadena"
programming_language: "C++"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/cpp/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# Por qué

La longitud de una cadena de texto es una información esencial en la programación. Saber la cantidad de caracteres que componen una cadena nos permite realizar diversas operaciones y manipulaciones de dicha cadena. En este artículo exploraremos cómo encontrar la longitud de una cadena en C++.

## Cómo hacerlo
En C++, podemos encontrar la longitud de una cadena utilizando la función `length()` de la librería `string`. Veamos un ejemplo de cómo se utilizaría esta función en un programa:

```C++
#include <iostream>
#include <string>

using namespace std;

int main() {
    string cadena = "Hola mundo";
    int longitud = cadena.length();
    cout << "La longitud de la cadena es: " << longitud << endl;
    return 0;
}
```

En este caso, la salida del programa sería:
```
La longitud de la cadena es: 10
```

También podemos utilizar la función `size()`, que es equivalente a `length()` y realiza la misma tarea. A continuación, veremos un ejemplo:

```C++
#include <iostream>
#include <string>

using namespace std;

int main() {
    string cadena = "Hola mundo";
    int longitud = cadena.size();
    cout << "La longitud de la cadena es: " << longitud << endl;
    return 0;
}
```

La salida del programa sería la misma que en el ejemplo anterior:
```
La longitud de la cadena es: 10
```

Además, es importante mencionar que la longitud de una cadena incluye todos los caracteres, incluso los espacios en blanco y signos de puntuación.

## Profundizando
Existen diferentes métodos para encontrar la longitud de una cadena en C++. A parte de las ya mencionadas, también podemos utilizar la función `strlen()` de la librería `cstring`, que es propia del lenguaje C. Sin embargo, esta función sólo puede ser utilizada con cadenas de texto clásicas, no con objetos de la clase `string`.

También es importante tener en cuenta que la longitud de una cadena puede variar dependiendo del tipo de codificación utilizado. Si estamos trabajando con cadenas en diferentes idiomas, como por ejemplo el chino o el ruso, es necesario utilizar codificaciones especiales como `wchar_t` para encontrar la longitud correcta.

## Ver también
- [Función length en cppreference](https://en.cppreference.com/w/cpp/string/basic_string/length)
- [Función size en cppreference](https://en.cppreference.com/w/cpp/string/basic_string/size)
- [Función strlen en cplusplus.com](https://www.cplusplus.com/reference/cstring/strlen/)
- [Codificaciones de caracteres en cplusplus.com](https://www.cplusplus.com/reference/cstdlib/mblen/)