---
title:    "C++: Encontrando la longitud de una cadena"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/cpp/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Por qué

En programación, es común trabajar con cadenas de caracteres o strings. Una de las operaciones básicas que podemos realizar con ellas es determinar su longitud, es decir, cuántos caracteres contiene. A continuación, explicaré por qué es importante saber cómo encontrar la longitud de una string en C++.

## Cómo hacerlo

Para encontrar la longitud de una string en C++, utilizamos la función integrada `length()`, que nos devuelve un valor entero correspondiente al número de caracteres del string. Veamos un ejemplo:

```C++
#include <iostream>
using namespace std;

int main() {
    string miString = "¡Hola Mundo!";
    int longitud = miString.length();

    cout << "La longitud de miString es de " << longitud << " caracteres." << endl;

    return 0;
}
```
**Output:**
```
La longitud de miString es de 11 caracteres.
```

También podemos utilizar la función `size()` de la misma manera, ya que es un alias de `length()` en C++. A continuación, un ejemplo donde pedimos al usuario que ingrese una string y mostramos su longitud:

```C++
#include <iostream>
using namespace std;

int main() {
    string miString;
    cout << "Ingrese una string: ";
    cin >> miString;
    int longitud = miString.size();

    cout << "La longitud de su string es de " << longitud << " caracteres." << endl;

    return 0;
}
```
**Output:**
```
Ingrese una string: Programar es divertido
La longitud de su string es de 21 caracteres.
```

## Profundizando

En realidad, encontrar la longitud de una string en C++ es una tarea sencilla y no requiere de mucha explicación adicional. Sin embargo, es importante destacar que el método `length()` no cuenta el carácter nulo (`\0`) al final del string, mientras que `size()` lo incluye en su cálculo. Además, en caso de que utilicemos un tipo de dato `char` en lugar de un objeto `string`, podemos obtener la longitud del mismo utilizando la función `strlen()` de la librería `cstring`.

## Ver también

- [Documentación de la función length() en cplusplus.com](https://www.cplusplus.com/reference/string/string/length/)
- [Ejemplos de la función size() en geeksforgeeks.org](https://www.geeksforgeeks.org/size-function-in-c-stl/)