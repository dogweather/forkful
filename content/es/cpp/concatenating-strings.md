---
title:                "C++: Uniendo cadenas de caracteres"
simple_title:         "Uniendo cadenas de caracteres"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/cpp/concatenating-strings.md"
---

{{< edit_this_page >}}

## Por qué

La concatenación de cadenas es una técnica esencial en la programación que permite unir varias cadenas de texto en una sola. Es una habilidad útil para manipular y mostrar información en programas de C++, por lo que aprender a concatenar cadenas es una habilidad valiosa para cualquier desarrollador.

## Cómo hacerlo

La sintaxis básica para concatenar cadenas en C++ es usando el operador `+`. Aquí hay un ejemplo de código que une dos cadenas y las imprime en la consola:

```C++
#include <iostream>
using namespace std;

int main() {
    string nombre = "Pedro";
    string apellido = "González";
    
    cout << nombre + " " + apellido << endl;
    // Salida: Pedro González
    
    return 0;
}
```

El operador `+` también se puede utilizar para unir más de dos cadenas a la vez. Además, es posible utilizar variables en lugar de cadenas fijas, lo que hace que la concatenación sea dinámica y adaptable a diferentes valores.

```C++
#include <iostream>
using namespace std;

int main() {
    string producto = "iPhone";
    int cantidad = 3;
    string orden = "Tu orden de " + to_string(cantidad) + " " + producto + "s ha sido confirmada.";
    
    cout << orden << endl;
    // Salida: Tu orden de 3 iPhones ha sido confirmada.
    
    return 0;
}
```

Otra forma de concatenar cadenas es usando la función `concat()` de la biblioteca `string`. Esta función puede unir múltiples cadenas y variables en una sola cadena. Aquí hay un ejemplo de cómo usarlo:

```C++
#include <iostream>
#include <string>
using namespace std;

int main() {
    string mensaje = "¡Hola!";
    string saludo = "¿Cómo estás?";
    int edad = 25;
    
    string conversacion = concat(mensaje, " ", saludo, " Soy Pedro y tengo ", to_string(edad), " años.");
    
    cout << conversacion << endl;
    // Salida: ¡Hola! ¿Cómo estás? Soy Pedro y tengo 25 años.
    
    return 0;
}
```

## Profundizando

Aunque el operador `+` y la función `concat()` son las formas más comunes de concatenar cadenas en C++, también existen otras funciones y métodos que pueden ser utilizados para este propósito. Por ejemplo, la función `append()` puede ser utilizada para agregar una cadena al final de otra cadena ya existente.

También es importante tener en cuenta la eficiencia al concatenar cadenas en C++. Debido a la forma en que se gestionan las cadenas en memoria, concatenar cadenas repetidamente en un bucle puede ser un proceso lento e ineficiente. Una forma de solucionar esto es utilizando un `stringstream`, que es una clase de la biblioteca `sstream` que permite construir una cadena mediante la adición de variables y cadenas sin tener que crear una nueva cadena en cada paso.

## Ver también

- [Documentación oficial de C++ sobre cadenas](https://www.cplusplus.com/reference/string/string/)
- [Tutorial de concatenación de cadenas en C++](https://www.guru99.com/concatenation-c-plus-plus.html)
- [Video tutorial sobre concatenación de cadenas en C++](https://www.youtube.com/watch?v=FAzt9lLgB8k)