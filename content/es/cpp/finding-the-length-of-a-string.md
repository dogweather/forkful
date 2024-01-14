---
title:    "C++: Encontrando la longitud de una cadena"
keywords: ["C++"]
---

{{< edit_this_page >}}

## Por qué

A menudo, en la programación, necesitamos saber la longitud de una cadena de texto para realizar ciertas acciones, como validar su entrada o realizar operaciones matemáticas. En esta entrada, aprenderemos cómo encontrar la longitud de una cadena de texto en C++, utilizando diferentes métodos.

## Cómo hacerlo

En C++, podemos obtener la longitud de una cadena de texto utilizando la función `strlen()`, que se encuentra en la biblioteca `cstring` de la librería estándar de C++. Esta función recibe una cadena de texto como parámetro y devuelve un entero que representa la cantidad de caracteres en esa cadena.

```C++
#include <iostream>
#include <cstring>
using namespace std;

int main() {
    // Definir una cadena de texto
    char miCadena[] = "Hola mundo";

    // Obtener la longitud de la cadena
    int longitud = strlen(miCadena);

    // Imprimir la longitud de la cadena
    cout << "La longitud de la cadena es: " << longitud << endl;
    
    return 0;
}
```

**Output:**

`La longitud de la cadena es: 10`

También podemos usar el método `length()` de la clase `string` de la librería estándar de C++, que nos permite obtener la longitud de una cadena de texto de una forma más intuitiva y sencilla.

```C++
#include <iostream>
#include <string>
using namespace std;

int main() {
    // Definir una cadena de texto
    string miCadena = "Hola mundo";

    // Obtener la longitud de la cadena
    int longitud = miCadena.length();

    // Imprimir la longitud de la cadena
    cout << "La longitud de la cadena es: " << longitud << endl;
    
    return 0;
}
```

**Output:**

`La longitud de la cadena es: 10`

## Profundizando

Ambos métodos, `strlen()` y `length()`, utilizan diferentes enfoques para encontrar la longitud de una cadena de texto. Mientras que `strlen()` cuenta la cantidad de caracteres hasta encontrar el caracter nulo (`\0`), `length()` utiliza un atributo interno de la clase `string` que almacena la longitud de la cadena. Por lo tanto, si tenemos una cadena de texto que incluye un caracter nulo en algún lugar previo al final de la cadena, `strlen()` nos devolverá una longitud incorrecta, mientras que `length()` nos dará la longitud correcta.

Otra diferencia importante entre estos dos métodos es que `length()` sólo puede ser utilizado con cadenas de texto del tipo `string` de C++, mientras que `strlen()` funciona con cualquier tipo de cadena de texto definida en C++ (como `char[]` o `char*`).

## Véase también

- [Documentación de `strlen()` en cplusplus.com](http://www.cplusplus.com/reference/cstring/strlen/)
- [Documentación de `length()` en cplusplus.com](http://www.cplusplus.com/reference/string/string/length/)