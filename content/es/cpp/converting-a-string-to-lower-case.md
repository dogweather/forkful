---
title:                "C++: Convirtiendo una cadena a minúsculas"
simple_title:         "Convirtiendo una cadena a minúsculas"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/cpp/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Por qué

A menudo, cuando trabajamos con texto en nuestro programa, es necesario convertir cadenas de caracteres a minúsculas. Ya sea para comparar dos palabras con diferentes capitalizaciones o para asegurarse de que los datos ingresados sean consistentes, el uso de letras minúsculas puede ser esencial. En esta entrada del blog, hablaremos sobre cómo convertir cadenas a minúsculas en C++.

## Cómo hacerlo

La conversión de una cadena a minúsculas en C++ es muy sencilla. Primero, necesitamos incluir la librería de cadenas `string` en nuestro programa. Luego, podemos utilizar la función `tolower()` que convierte una letra en minúscula. Podemos aplicar esta función a cada carácter de la cadena utilizando un bucle `for` y así obtener la cadena en minúsculas completa.

```C++
#include <string>

std::string lowercase(std::string str) {
    for(int i = 0; i < str.size(); i++) {
        str[i] = tolower(str[i]);
    }
    return str;
}

int main() {
    std::string str = "HOLA MUNDO";
    std::cout << lowercase(str) << std::endl; // salida: hola mundo
    return 0;
}
```

## Profundizando

Una cosa importante a tener en cuenta es que la función `tolower()` solo funciona con caracteres individuales. Si queremos convertir una cadena entera a minúsculas, debemos aplicarla a cada carácter de la cadena como se muestra en el ejemplo anterior.

Además, también es posible utilizar la función `transform()` de la librería `algorithm` para convertir una cadena completa a minúsculas. Esta función toma como argumentos el inicio y el final de la cadena, y una función que se aplicará a cada carácter. En este caso, utilizamos la función `tolower()`.

```C++
#include <string>
#include <algorithm>

std::string lowercase(std::string str) {
    std::transform(str.begin(), str.end(), str.begin(), ::tolower);
    return str;
}

int main() {
    std::string str = "HOLA MUNDO";
    std::cout << lowercase(str) << std::endl; // salida: hola mundo
    return 0;
}
```

Otra cosa importante a tener en cuenta es que la conversión solo se aplicará a letras del alfabeto. Cualquier otro carácter, como números o símbolos, permanecerán sin cambios.

## Ver también

Si quieres saber más sobre el manejo de cadenas en C++, te recomendamos los siguientes enlaces:

- [Documentación oficial de C++ sobre la librería de cadenas](https://devdocs.io/cpp/string/basic_string)
- [Tutorial de Programiz sobre la manipulación de cadenas en C++](https://www.programiz.com/cpp-programming/strings)
- [Preguntas y respuestas relacionadas con cadenas en Stack Overflow en Español](https://es.stackoverflow.com/questions/tagged/c%2b%2b?tab=Votes&pagesize=15&q=%5bstring%5d+lower)