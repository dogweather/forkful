---
title:    "C++: Capitalizando una cadena"
keywords: ["C++"]
---

{{< edit_this_page >}}

## ¿Por qué capitalizar una cadena de texto?

Al escribir código en C++, a menudo encontramos la necesidad de convertir una cadena de texto a mayúsculas para mejorar la legibilidad o para realizar comparaciones de cadenas. Afortunadamente, esto se puede lograr fácilmente con solo unas pocas líneas de código.

## Cómo capitalizar una cadena de texto en C++

Para capitalizar una cadena de texto en C++, podemos usar la función `std::transform()` junto con la función `std::toupper()` de la biblioteca `cctype` de C++. Primero, necesitamos incluir la biblioteca en nuestro código con la siguiente línea:

``` C++
#include <cctype>
```

Luego, podemos usar la función `std::transform()` para iterar a través de cada carácter de la cadena y aplicar la función `std::toupper()` para convertirlo en mayúscula. El siguiente ejemplo muestra cómo capitalizar una cadena llamada `myString` y luego imprimir el resultado en la consola:

``` C++
std::string myString = "hola mundo";
std::transform(myString.begin(), myString.end(), myString.begin(), std::toupper);
std::cout << myString << std::endl;
```

La salida de este código sería:

``` C++
HOLA MUNDO
```

Podemos ver que la función `std::transform()` modificó cada carácter de la cadena `myString` a mayúscula, lo que nos permite capitalizar la cadena completa.

## Profundizando en la capitalización de cadenas de texto

Cuando se trata de capitalizar cadenas de texto, debemos tener en cuenta que puede haber variantes de mayúsculas y minúsculas en diferentes idiomas. Por ejemplo, en español, la letra 'ñ' también se puede presentar en mayúscula como 'Ñ'. Por lo tanto, debemos asegurarnos de que nuestros programas manejen correctamente estos casos.

Además, también debemos tener en cuenta que al capitalizar una cadena, estamos reemplazando los caracteres originales con caracteres en mayúscula, lo que podría afectar a cualquier lógica o verificaciones futuras que dependan de la cadena original.

## Véase también

- [Documentación de std::transform](https://www.cplusplus.com/reference/algorithm/transform/)
- [Documentación de std::toupper](https://www.cplusplus.com/reference/cctype/toupper/)