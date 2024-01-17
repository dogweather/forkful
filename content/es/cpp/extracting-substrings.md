---
title:                "Extrayendo subcadenas"
html_title:           "C++: Extrayendo subcadenas"
simple_title:         "Extrayendo subcadenas"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/cpp/extracting-substrings.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?
Extraer subcadenas en programación es el proceso de obtener una parte de una cadena de texto más grande. Los programadores lo hacen para manipular y trabajar con cadenas más pequeñas de una manera más eficiente.

## ¿Cómo hacerlo?
En C++, se utilizan dos funciones para extraer subcadenas: ```substr()``` y ```copy()```. Ambas funciones tienen parámetros que le indican a la función qué parte de la cadena desea extraer. Un ejemplo de código sería el siguiente:

```
#include <iostream>
#include <string>
using namespace std;

int main() {

    string cadena = "Hola mundo!";
    string subcadena = cadena.substr(5, 5); // Extrae la subcadena "mundo"
    cout << subcadena << endl;
    char buffer[6];
    cadena.copy(buffer, 6, 5); // Copia la subcadena " mundo" al buffer
    buffer[5] = '\0'; // Agrega el carácter de fin de cadena al buffer
    cout << buffer << endl;
    return 0;
}
```
**Resultado:**
```
mundo
 mundo
```

## Profundizando
El uso de subcadenas se ha vuelto cada vez más importante con el aumento en el uso de cadenas de texto en la programación. Antes, los programadores se veían obligados a trabajar con cadenas completas, lo que podía ser ineficiente. Hoy en día, hay varias formas de extraer subcadenas en C++, incluyendo las funciones de la librería ```string``` y las expresiones regulares.

## Ver también
- [Documentación de la función ```substr()``` en cplusplus.com](http://www.cplusplus.com/reference/string/string/substr/)
- [Documentación de la función ```copy()``` en cplusplus.com](http://www.cplusplus.com/reference/string/basic_string/copy/)
- [Ejemplos de expresiones regulares en C++](https://www.geeksforgeeks.org/regular-expressions-c/)