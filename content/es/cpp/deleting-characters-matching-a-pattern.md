---
title:                "C++: Eliminando caracteres que coinciden con un patrón."
programming_language: "C++"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/cpp/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## ¿Por qué borrar caracteres que coinciden con un patrón?

A menudo, cuando trabajamos con cadenas de texto, podemos encontrarnos con la necesidad de eliminar ciertos caracteres que coinciden con un patrón específico. Esto puede ser útil si estamos limpiando datos o filtrando información no deseada. Afortunadamente, en C++ existen varias formas de realizar esta tarea de manera eficiente.

## Cómo hacerlo

Existen diferentes maneras de eliminar caracteres que coinciden con un patrón en C++. Aquí presentaremos dos opciones: utilizando la función `erase()` y utilizando expresiones regulares.

### Utilizando la función `erase()`

La función `erase()` nos permite eliminar un conjunto de caracteres de una cadena de texto, indicando el índice de inicio y la cantidad de caracteres a borrar. Por ejemplo:

```C++
#include <iostream>
#include <string>
using namespace std;

string palabra = "programación";
palabra.erase(3, 6); // elimina los caracteres desde la posición 3 hasta la posición 6
cout << palabra; // muestra "pama"
```

En este ejemplo, la palabra "programación" se convierte en "pama" al eliminar los caracteres que van desde la posición 3 (incluida) hasta la posición 6 (no incluida).

### Utilizando expresiones regulares

Las expresiones regulares son patrones que nos permiten buscar y manipular cadenas de texto de manera más avanzada. En C++, podemos utilizar la librería `<regex>` para trabajar con expresiones regulares.

Por ejemplo, si queremos eliminar todas las vocales de una palabra, podemos utilizar la función `regex_replace()`, indicando como parámetro el patrón a buscar (en este caso, las vocales) y el carácter de reemplazo (en este caso, una cadena vacía):

```C++
#include <iostream>
#include <string>
#include <regex>
using namespace std;

string palabra = "programación";
regex vocales("[aeiou]");
palabra = regex_replace(palabra, vocales, ""); // elimina todas las vocales de la palabra
cout << palabra; // muestra "prgrmcn"
```

## Profundizando en la eliminación de caracteres

Existen muchas otras funciones y métodos en C++ que nos permiten eliminar caracteres que coinciden con un patrón, como por ejemplo `substr()`, `replace()` y `remove_if()`. Cada una de ellas tiene sus ventajas y es importante familiarizarse con ellas para poder elegir la mejor opción para cada caso.

## Ver también

- [Documentación de C++ sobre la función `erase()`](https://es.cppreference.com/w/cpp/string/basic_string/erase)
- [Tutoriales de C++ sobre expresiones regulares](https://www.tutorialspoint.com/cpp_standard_library/cpp_regular_expressions.htm)