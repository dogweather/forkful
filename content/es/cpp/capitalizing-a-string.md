---
title:                "C++: Capitalizando una cadena"
simple_title:         "Capitalizando una cadena"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/cpp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Por qué

Cuando se trabaja con cadenas de texto, a veces es necesario cambiar el formato de las mismas para que luzcan más legibles. En este caso, capitalizar una cadena puede ser útil para resaltar ciertas palabras o mejorar la presentación de datos en una interfaz de usuario.

## Cómo hacerlo

En lenguaje de programación C++, hay varias formas de capitalizar una cadena. Una de las más sencillas es utilizando la función `toupper()` de la librería `cctype`. Esta función convierte todos los caracteres de una cadena a mayúsculas.

```C++
#include <iostream>
#include <cstring>
#include <cctype>

int main() {
  char str[] = "Hola, mundo!";
  
  std::cout << "Cadena original: " << str << std::endl;
  
  // Convertir todo a mayúsculas
  for (int i = 0; i < strlen(str); i++) {
    str[i] = toupper(str[i]);
  }
  
  std::cout << "Cadena capitalizada: " << str << std::endl;
  
  return 0;
}
```

**Output:**

`Cadena original: Hola, mundo!`

`Cadena capitalizada: HOLA, MUNDO!`

Otra forma de realizar esta tarea es utilizando la función `transform()` de la librería `algorithm`. Esta función permite aplicar una transformación a cada elemento de un rango dado, utilizando una función especificada. En este caso, podemos utilizar la función `toupper()` para convertir cada caracter a mayúscula.

```C++
#include <iostream>
#include <string>
#include <algorithm>
#include <cctype>

int main() {
  std::string str = "Hola, mundo!";
  
  std::cout << "Cadena original: " << str << std::endl;
  
  // Convertir todo a mayúsculas
  std::transform(str.begin(), str.end(), str.begin(), ::toupper);
  
  std::cout << "Cadena capitalizada: " << str << std::endl;
  
  return 0;
}
```

**Output:**

`Cadena original: Hola, mundo!`

`Cadena capitalizada: HOLA, MUNDO!`

## Profundizando

Además de las formas mencionadas anteriormente, también es posible crear una función propia para capitalizar una cadena. Esta función podría tomar en cuenta ciertas excepciones, como por ejemplo palabras que deben permanecer en minúsculas (como artículos o preposiciones).

También es importante tener en cuenta que al utilizar `toupper()`, solo se convierten los caracteres en función del alfabeto inglés. Si necesitamos capitalizar cadenas con caracteres especiales o caracteres de otros idiomas, deberemos utilizar otras funciones como `std::locale::toupper()`.

## Ver también

- Referencia de la librería `cctype` en C++: https://en.cppreference.com/w/cpp/string/byte
- Tutorial de la librería `algorithm` en C++: https://www.geeksforgeeks.org/the-transform-function-in-c-stl/
- Referencia de la función `toupper()` en C++: https://www.cplusplus.com/reference/cctype/toupper/
- Documentación sobre la función `transform()` en C++: https://en.cppreference.com/w/cpp/algorithm/transform