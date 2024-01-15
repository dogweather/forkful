---
title:                "Uniendo cadenas de texto"
html_title:           "C++: Uniendo cadenas de texto"
simple_title:         "Uniendo cadenas de texto"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/cpp/concatenating-strings.md"
---

{{< edit_this_page >}}

## Por qué

Concatenar strings es una técnica útil en programación ya que permite unir varias cadenas de caracteres en una sola. Esto puede ser útil para crear mensajes complejos, imprimir información en pantalla o construir URLs dinámicas.

## Cómo hacerlo

En C++, la concatenación de strings se hace usando el operador "+" o la función "concat" en la biblioteca estándar "string". Aquí hay un ejemplo de cómo concatenar strings y mostrar su resultado en pantalla:

```C++
#include <iostream>
#include <string>

using namespace std;

int main() {
  // Declaración de dos strings
  string nombre = "Juan";
  string apellido = "Pérez";

  // Concatenación de strings usando el operador "+"
  string nombre_completo = nombre + " " + apellido;

  // Mostrando el resultado en pantalla
  cout << "El nombre completo es: " << nombre_completo << endl;

  // Concatenación de strings usando la función "concat"
  string mensaje = "Hola, " + nombre + " " + apellido + ", ¿cómo estás?";

  // Mostrando el resultado en pantalla
  cout << "El mensaje es: " << mensaje << endl;

  return 0;
}
```

El output de este código sería:

```
El nombre completo es: Juan Pérez
El mensaje es: Hola, Juan Pérez, ¿cómo estás?
```

## Profundizando

En C++, los strings son objetos que contienen una secuencia de caracteres. Cuando se realiza la concatenación de strings, lo que realmente se está haciendo es unir las secuencias de caracteres en un solo objeto.

Es importante tener en cuenta que la concatenación de strings puede ser costosa en términos de tiempo y memoria en comparación con otras operaciones. Por lo tanto, es recomendable utilizarla con moderación y considerar otras alternativas si se necesita un rendimiento más rápido.

## Ver también

- [Documentación oficial de C++ sobre la concatenación de strings](https://en.cppreference.com/w/cpp/string/basic_string/operator%2B)
- [Ejemplos de aplicaciones de concatenación de strings en programación](https://www.geeksforgeeks.org/concatenation-string-using-stdstring-concat-function-c/)