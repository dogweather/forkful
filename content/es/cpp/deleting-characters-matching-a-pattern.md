---
title:    "C++: Eliminando caracteres que coinciden con un patrón"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/cpp/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Por qué

Eliminar caracteres que coinciden con un patrón es una tarea común en la programación. Ya sea para limpiar datos, filtrar entradas o manipular cadenas de texto, esta técnica puede ser útil en una variedad de situaciones.

## Cómo hacerlo

```C++
#include <iostream>
#include <string>
#include <algorithm>
using namespace std;

int main() {
   string str = "ababacdaba"; // cadena de ejemplo
   char pattern = 'a'; // patrón a eliminar

   cout << "Cadena original: " << str << endl;

   // eliminando caracteres
   str.erase(remove(str.begin(), str.end(), pattern), str.end());

   cout << "Cadena después de eliminar el patrón '" << pattern << "': " << str << endl;
   return 0;
}
```

**Salida:**

```
Cadena original: ababacdaba
Cadena después de eliminar el patrón 'a': bbcdb
```

En este código, se utiliza la función `erase()` de la biblioteca `<algorithm>` para eliminar todos los caracteres que coinciden con el patrón proporcionado. Esta función toma como parámetros los iteradores al inicio y al final de la sección de la cadena que se desea eliminar y devuelve un iterador al final de la cadena resultante.

Además, se usa también la función `remove()` para mover todos los elementos que no coinciden con el patrón al comienzo de la cadena y devuelve un iterador al primer elemento en el nuevo arreglo. Por último, se usan los iteradores devueltos por `remove()` y `erase()` para eliminar efectivamente los caracteres no deseados de la cadena.

## Profundizando

Este método de eliminar caracteres que coinciden con un patrón se basa en la función `remove_if()` de la biblioteca `<algorithm>`. Esta función también toma dos iteradores como parámetros pero permite una mayor flexibilidad al definir el patrón a eliminar a través de una función de predicado. Esto significa que se puede proporcionar una función que determine si un carácter debe ser eliminado o no, en lugar de especificar un carácter específico.

Además, la función `erase()` no solo se puede usar para eliminar caracteres, sino que también es útil para eliminar cualquier elemento en una cadena que cumpla cierta condición, como por ejemplo, eliminar valores duplicados en un arreglo.

## Ver también

- [Función `erase()` (en inglés)](https://www.cplusplus.com/reference/string/string/erase/)
- [Función `remove()` (en inglés)](https://www.cplusplus.com/reference/algorithm/remove/)
- [Función `remove_if()` (en inglés)](https://www.cplusplus.com/reference/algorithm/remove_if/)