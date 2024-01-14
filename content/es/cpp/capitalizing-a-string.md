---
title:                "C++: Convertir una cadena en mayúsculas"
programming_language: "C++"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/cpp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## ¿Por qué capitalizar una cadena?

Cuando trabajamos con cadenas de caracteres en nuestros programas, a menudo nos encontramos con la necesidad de capitalizarlas, es decir, cambiar la primera letra de cada palabra a mayúscula. Esto puede ser útil para fines de presentación, para asegurarse de que los nombres propios se vean correctamente o para seguir un formato específico. Afortunadamente, en C++, hay una forma sencilla de realizar esta tarea utilizando algunas funciones de la biblioteca estándar.

## Cómo hacerlo

Para capitalizar una cadena en C++, utilizamos las funciones `toupper()` y `tolower()`, que nos permiten convertir caracteres a mayúsculas o minúsculas respectivamente. A continuación, se muestra un ejemplo de cómo podemos utilizar estas funciones para capitalizar una cadena de caracteres:

```C++
#include <iostream>
#include <string>

using namespace std;

string capitalize(string str) { // Función que capitaliza una cadena
    for (int i = 0; i < str.length(); i++) { // Recorremos la cadena
        if (i == 0) // Si es la primera letra
            str[i] = toupper(str[i]); // La convertimos a mayúscula
        else if (str[i - 1] == ' ') // Si la letra anterior fue un espacio
            str[i] = toupper(str[i]); // Convertimos la actual a mayúscula
        // En caso contrario, no hacemos cambios
    }
    return str; // Retornamos la cadena ya capitalizada
}

int main() {
    string str = "hola mundo"; // Cadena original
    string capitalized = capitalize(str); // Cadena capitalizada
    cout << capitalized << endl; // Imprimimos: Hola Mundo
    return 0;
}
```

## Profundizando

Este ejemplo es solo una forma de capitalizar una cadena y puede ser mejorado. Por ejemplo, podríamos tener una función que reciba una cadena y un carácter como parámetros, y capitalice la primera letra de cada palabra que comience con ese carácter. También podríamos tomar en cuenta las excepciones, como las palabras en mayúsculas o la primera letra de una oración. Depende de nuestros requisitos y del nivel de complejidad que queramos agregar a la función.

## Ver también

- [toupper() en cplusplus.com](http://www.cplusplus.com/reference/cctype/toupper/)
- [tolower() en cplusplus.com](http://www.cplusplus.com/reference/cctype/tolower/)