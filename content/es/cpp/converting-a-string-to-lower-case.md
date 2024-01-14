---
title:    "C++: Convertir una cadena a minúsculas"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/cpp/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# Por qué

Convertir una cadena de texto a minúsculas es una tarea común en la programación de C++. Puede ser útil en situaciones donde se necesita estandarizar las entradas del usuario o cuando se necesita comparar cadenas de texto sin importar las mayúsculas y minúsculas.

## Cómo hacerlo

Para convertir una cadena de texto a minúsculas en C++, se puede utilizar la función integrada `tolower()` que se encuentra en la librería `<cctype>`. Esta función toma como parámetro un carácter y devuelve su equivalente en minúsculas. Por lo tanto, para convertir una cadena a minúsculas, se debe aplicar esta función a cada carácter de la cadena utilizando un bucle.

Un ejemplo de código podría ser el siguiente:

```C++
#include <iostream>
#include <cctype>

using namespace std;

int main() {
    string texto;
    cout << "Ingresa una cadena de texto: ";
    getline(cin, texto);

    // convertir todos los caracteres a minúsculas
    for(int i = 0; i < texto.length(); i++) {
        texto[i] = tolower(texto[i]);
    }

    cout << "La cadena en minúsculas es: " << texto << endl;

    return 0;
}
```

**Salida:**

```
Ingresa una cadena de texto: Hola Mundo!

La cadena en minúsculas es: hola mundo!
```

## Profundizando

El proceso de convertir una cadena de texto a minúsculas puede parecer simple, pero hay ciertos aspectos a considerar para obtener un resultado preciso. Por ejemplo, la función `tolower()` solo funciona con caracteres individuales, por lo que si se desea convertir una cadena de texto con caracteres especiales como letras acentuadas, se debe utilizar una librería externa o implementar un algoritmo personalizado.

Además, hay que tener en cuenta que en C++, los caracteres se representan en ASCII, por lo que al convertir una letra mayúscula a minúscula, se obtendrá un número diferente en la tabla ASCII. Por ejemplo, la letra "A" en mayúscula tiene un código ASCII de 65 y en minúscula tiene un código de 97. Esto puede afectar a la comparación de cadenas de texto en algunos casos.

## Ver también

- [Función tolower() en cplusplus.com](https://www.cplusplus.com/reference/cctype/tolower/)
- [Ejemplo de C++ para convertir una cadena a minúsculas](https://www.geeksforgeeks.org/uppercase-string-lowercase-string-cc/)
- [Artículo sobre el alfabeto ASCII](https://en.wikipedia.org/wiki/ASCII)