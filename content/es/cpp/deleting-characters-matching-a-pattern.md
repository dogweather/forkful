---
title:                "Eliminando caracteres que coinciden con un patrón."
html_title:           "C++: Eliminando caracteres que coinciden con un patrón."
simple_title:         "Eliminando caracteres que coinciden con un patrón."
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/cpp/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## ¿Por qué?

En la programación, a menudo es necesario realizar operaciones específicas en cadenas de texto. A veces, es necesario eliminar ciertos caracteres que coinciden con un patrón para obtener la información deseada. El uso de la función de eliminación de caracteres en C++ es una habilidad valiosa para cualquier programador.

## ¿Cómo hacerlo?

Para eliminar caracteres que coinciden con un patrón en C++, se puede usar la función `erase` de la clase `string`. Esta función acepta dos argumentos: la posición inicial desde donde se eliminarán los caracteres y la cantidad de caracteres a eliminar. Por ejemplo, si se tiene una cadena `'Hola mundo'` y se desea eliminar las primeras tres letras, el código se vería así:

```C++
string texto = "Hola mundo";
texto.erase(0,3);
cout << texto << endl;
```
**Salida: mundo**

También se puede usar la función `find_first_of` para encontrar la posición de un carácter específico en una cadena y luego usar esa posición en la función `erase`. Por ejemplo, si se desea eliminar todos los espacios en blanco de una cadena, el código se vería así:

```C++
string texto = "Hola mundo";
int pos = texto.find_first_of(" ");
texto.erase(pos, 1);
cout << texto << endl;
```
**Salida: Holamundo**

## Profundizando

La función `erase` de C++ también se puede utilizar para eliminar un rango de caracteres en una cadena. Se debe especificar la posición inicial y final del rango a eliminar. Además, se puede usar la función `replace` para reemplazar un rango de caracteres con una subcadena específica.

Además de las funciones mencionadas, también hay otras formas de eliminar caracteres que coinciden con un patrón en C++, como el uso de expresiones regulares con la biblioteca `<regex>`.

Otra técnica comúnmente utilizada es el uso de un bucle `for` para recorrer cada carácter de una cadena y eliminar los que coinciden con el patrón deseado.

En resumen, hay varias formas de eliminar caracteres que coinciden con un patrón en C++. Es importante tener conocimiento de estas técnicas para poder manejar correctamente las cadenas de texto en la programación.

## Ver también

- [C++ string erase documentation](https://www.cplusplus.com/reference/string/string/erase/)
- [C++ string replace documentation](https://www.cplusplus.com/reference/string/string/replace/)
- [C++ regular expressions tutorial](https://www.cplusplus.com/articles/whUwvCM9/)
- [Looping through a string in C++](https://www.programiz.com/cpp-programming/examples/loop-string)