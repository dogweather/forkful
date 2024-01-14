---
title:                "C++: Eliminando caracteres que coincidan con un patrón"
simple_title:         "Eliminando caracteres que coincidan con un patrón"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/cpp/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Por qué

En programación, es común encontrarse con la necesidad de eliminar ciertos caracteres que coinciden con un patrón específico en una cadena de texto. Esto puede ser útil para limpiar datos o para buscar y reemplazar ciertas palabras. En esta publicación, exploraremos cómo podemos lograr esto en C++.

## Cómo hacerlo

Para eliminar caracteres que coinciden con un patrón en una cadena, utilizaremos la función `erase` de la clase `string`. Esta función toma dos argumentos: la posición del carácter a eliminar y la cantidad de caracteres a eliminar. Podemos utilizar un bucle `for` para iterar a través de la cadena y eliminar todos los caracteres que coincidan con nuestro patrón. Veamos un ejemplo de código:

```C++
#include <iostream>
#include <string>
using namespace std;

int main() {
    // Definimos nuestra cadena de texto
    string texto = "Hola mundo!";

    // Iteramos a través de la cadena y eliminamos las letras "o"
    for (int i = 0; i < texto.length(); i++) {
        if (texto[i] == 'o') {
            texto.erase(i, 1);
        }
    }

    // Imprimimos el resultado
    cout << texto << endl;
    // Output: Hla mund!
}
```

En este ejemplo, utilizamos la función `length` para obtener el tamaño de la cadena y el método `erase` para eliminar las letras "o". Este mismo proceso se puede adaptar para eliminar cualquier otro patrón de caracteres en una cadena.

## Profundizando

Si queremos ser más específicos en nuestro patrón y solo eliminar ciertos caracteres que coinciden con una expresión regular, podemos utilizar la librería `regex` de C++. Por ejemplo, podemos utilizar `regex_replace` para buscar y reemplazar palabras enteras en una cadena. Aquí está un ejemplo de código:

```C++
#include <iostream>
#include <string>
#include <regex>
using namespace std;

int main() {
    // Definimos nuestra cadena de texto
    string texto = "Hola amigos!";

    // Reemplazamos la palabra "amigos" por "compañeros"
    texto = regex_replace(texto, regex("amigos"), "compañeros");

    // Imprimimos el resultado
    cout << texto << endl;
    // Output: Hola compañeros!
}
```

En este ejemplo, la función `regex_replace` toma tres argumentos: la cadena original, la expresión regular para buscar y la cadena de reemplazo. Esto nos permite ser más precisos en nuestra eliminación de caracteres.

## Ver también

- [Documentación de la clase string en C++](https://www.cplusplus.com/reference/string/string/)
- [Documentación de la librería regex en C++](https://www.cplusplus.com/reference/regex/)