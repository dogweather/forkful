---
title:                "Buscando y reemplazando texto"
html_title:           "C++: Buscando y reemplazando texto"
simple_title:         "Buscando y reemplazando texto"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/cpp/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## ¿Por qué hacer búsqueda y reemplazo de texto?

La búsqueda y reemplazo de texto es una herramienta útil para editar y modificar rápidamente grandes cantidades de texto en un programa. Puede ayudar a ahorrar tiempo y esfuerzo al realizar cambios en múltiples líneas de código de manera eficiente.

## Cómo hacer la búsqueda y reemplazo de texto en C++

Para realizar búsqueda y reemplazo de texto en C++, se utiliza la función `replace` junto con un objeto de tipo `string` y los parámetros de búsqueda y reemplazo. Por ejemplo:

```C++
string str = "hola mundo";
replace(str, "hola", "adiós");
cout << str;
```

El resultado de la ejecución sería "adiós mundo". También se puede utilizar la función `find` para buscar una cadena específica en un objeto de tipo `string` y luego utilizar la función `replace` para reemplazarla. Por ejemplo:

```C++
string str = "C++ es divertido";
int pos = str.find("divertido");
replace(str, "divertido", "genial");
cout << str;
```

La salida sería "C++ es genial". También se pueden utilizar expresiones regulares para realizar búsquedas y reemplazos más complejos en un texto.

## Inmersión profunda en la búsqueda y reemplazo de texto

La función `replace` utiliza una cadena o una expresión regular para buscar una coincidencia dentro del objeto `string` y reemplazarla con otra cadena o expresión. También se pueden utilizar parámetros opcionales para especificar la posición inicial de la búsqueda y la cantidad de caracteres a reemplazar. Además, también se pueden utilizar otras funciones como `find_first_of` y `find_last_of` para buscar una coincidencia de cualquier carácter en una lista de caracteres.

## Ver también

- [Documentación de la función `replace` en C++ ](https://www.cplusplus.com/reference/string/string/replace/)

- [Tutorial de expresiones regulares en C++](https://www.tutorialspoint.com/cplusplus/cpp_regular_expressions.htm)

- [Función `find` en C++](https://www.cplusplus.com/reference/string/basic_string/find/)

- [Funciones de búsqueda en C++](https://www.learncpp.com/cpp-tutorial/searching-for-strings-in-a-string/)