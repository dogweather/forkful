---
title:                "C++: Utilizando expresiones regulares"
programming_language: "C++"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/cpp/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Por qué utilizar expresiones regulares en C++

Las expresiones regulares son una herramienta poderosa para la manipulación de cadenas de texto en cualquier lenguaje de programación, incluyendo C++. Con su ayuda, podemos buscar patrones específicos de caracteres y realizar diferentes acciones en función de los resultados obtenidos. Esto nos permite realizar tareas más complejas y ahorrar tiempo y esfuerzo en la manipulación de cadenas de texto.

## Cómo utilizar expresiones regulares en C++

Para utilizar expresiones regulares en C++, primero debemos incluir la biblioteca "regex". A continuación, definimos un objeto de tipo "regex" con el patrón que deseamos buscar. Luego, usamos la función "regex_match" para buscar el patrón en una cadena de texto y devolver un valor booleano que indica si se encuentra o no. A continuación, se muestra un ejemplo de cómo utilizar expresiones regulares en C++:

```C++
#include <iostream>
#include <regex>

int main() {
  std::string texto = "¡Hola, este es un ejemplo de texto con un número de teléfono! El número es 123-456-7890.";
  std::regex patron("[0-9]{3}-[0-9]{3}-[0-9]{4}"); // patrón para buscar números de teléfono
  bool encontrado = std::regex_match(texto, patron);
  
  if (encontrado) {
    std::cout << "¡Se encontró un número de teléfono en el texto!" << std::endl;
  } else {
    std::cout << "No se encontró ningún número de teléfono." << std::endl;
  }
  
  return 0;
}
```

La salida de este código será: "¡Se encontró un número de teléfono en el texto!". Esto demuestra cómo podemos utilizar expresiones regulares para buscar patrones en una cadena de texto y tomar decisiones basadas en los resultados obtenidos.

## Profundizando en el uso de expresiones regulares

Las expresiones regulares en C++ también nos ofrecen una variedad de funciones para realizar diferentes acciones en una cadena de texto. Podemos usar "regex_search" para buscar el patrón en todo el texto y devolver un resultado, "regex_replace" para reemplazar el patrón con una cadena de texto diferente, y muchas más. También podemos utilizar metacaracteres, como "." para representar cualquier carácter y "?" para hacer que un carácter sea opcional en el patrón.

Es importante tener en cuenta que la sintaxis y las funciones de las expresiones regulares pueden variar ligeramente dependiendo del lenguaje de programación que estemos utilizando. Por lo tanto, es importante consultar la documentación específica del lenguaje que estamos utilizando para obtener una comprensión completa y correcta del uso de expresiones regulares.

## Ver también
- [Documentación de C++ sobre expresiones regulares](https://www.cplusplus.com/reference/regex/)
- [Tutorial de expresiones regulares en C++](https://www.tutorialspoint.com/cpp_standard_library/regex.htm)
- [Ejemplos prácticos de uso de expresiones regulares en C++](https://www.GeeksForGeeks.org/regular-expressions-in-Cpp/)

¡Ahora que hemos aprendido sobre el uso de expresiones regulares en C++, es hora de poner en práctica nuestro conocimiento en nuestros proyectos! ¡Buena suerte!