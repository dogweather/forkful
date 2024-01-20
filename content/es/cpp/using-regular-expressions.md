---
title:                "Usando expresiones regulares"
html_title:           "Go: Usando expresiones regulares"
simple_title:         "Usando expresiones regulares"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/cpp/using-regular-expressions.md"
---

{{< edit_this_page >}}

## ¿Qué y Por qué?
Las expresiones regulares (regex) son una herramienta para buscar patrones en texto. Los programadores las usan para encontrar, reemplazar y manipular estos patrones de manera eficiente y flexible.

## ¿Cómo?
Aquí te muestro cómo utilizar regex en C++ para buscar una secuencia de dígitos en un string:

```C++
#include <regex>
#include <string>

int main()
{
   std::string s = "abc123def456ghi789";
   std::regex e ("\\d+");

   // Buscando la regex en el string
   bool match = std::regex_search(s, e);
   
   std::cout << (match ? "Match encontrado" : "Match no encontrado");
   return 0;
}
```
Con este código, la salida será: `Match encontrado`, ya que hemos averiguado que existen dígitos en la cadena.

## Inmersión Profunda
Las regex tienen sus raíces en la teoría de autómatas y la teoría formal del lenguaje. Su uso es muy extendido en los lenguajes de programación por su flexibilidad y eficiencia al manipular cadenas de texto.

Alternativas a su uso pueden ser el uso de funciones básicas de manipulación de strings o el diseño de algoritmos personalizados, pero estos enfoques pueden ser más engorrosos y menos eficientes.

La implementación de regex en C++ utiliza una clase del estándar STL, std::regex, que permite utilizarlas directamente en tu código. C++ proporciona múltiples funciones para buscar y manipular cadenas de texto utilizando regex, como regex_search y regex_replace.

## Ver También
Para buscar más información sobre el uso y las capacidades de las expresiones regulares, puedes consultar los siguientes recursos:

- Documentación oficial de C++ [regex](http://www.cplusplus.com/reference/regex/)
- Tutorial básico de [regex en C++](https://www.geeksforgeeks.org/regex-in-cpp/)
- Guía práctica de [regex](https://regexr.com/)