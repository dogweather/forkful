---
title:    "C++: Extracción de subcadenas"
keywords: ["C++"]
---

{{< edit_this_page >}}

## ¿Por qué extraer subcadenas en C++?

Extraer subcadenas en C++ es una habilidad útil para cualquier programador. Permite manipular cadenas de texto y obtener partes específicas de ellas, lo que es especialmente útil cuando se trabaja con datos de entrada o salida.

## Cómo hacerlo

Para extraer una subcadena en C++, se puede utilizar la función `substr()` que se encuentra en la biblioteca estándar `string`. Esta función toma dos argumentos: el índice inicial y la longitud de la subcadena que se desea extraer.

```C++
// Ejemplo de código para extraer una subcadena
#include <iostream>
#include <string>
using namespace std;

int main() {
    // Creamos una cadena
    string miCadena = "Hola mundo";

    // Extraemos la subcadena que va desde el segundo hasta el cuarto carácter
    string subcadena = miCadena.substr(1, 3);

    // Imprimimos la subcadena
    cout << "La subcadena es: " << subcadena << endl;

    return 0;
}
```
**Salida:**
```
La subcadena es: ola
```

## Profundizando

La función `substr()` utiliza dos parámetros para determinar qué subcadena se debe extraer: el índice inicial y la longitud. El índice inicial indica desde qué posición de la cadena se debe comenzar a extraer la subcadena, y la longitud indica cuántos caracteres se deben extraer a partir de esa posición. Por ejemplo, si el índice inicial es 0 y la longitud es 3, se extraerán los primeros 3 caracteres de la cadena.

Además, es importante tener en cuenta que la cuenta de los índices en C++ comienza en 0. Esto significa que el primer carácter de una cadena tiene un índice de 0, el segundo tiene un índice de 1, y así sucesivamente.

Otra cosa a tener en cuenta es que si se omite el segundo parámetro (la longitud), la función `substr()` extraerá la subcadena desde el índice inicial hasta el final de la cadena.

## Ver también

- [Referencia de la función `substr()` en cplusplus.com](https://www.cplusplus.com/reference/string/string/substr/)
- [Tutorial sobre cadenas en C++ en Programiz](https://www.programiz.com/cpp-programming/string)