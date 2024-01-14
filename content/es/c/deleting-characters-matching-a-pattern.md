---
title:                "C: Eliminando caracteres que coinciden con un patrón"
simple_title:         "Eliminando caracteres que coinciden con un patrón"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Por qué

A veces, en programación, nos encontramos con la necesidad de eliminar ciertos caracteres de una cadena de texto que coinciden con un patrón específico. Ya sea para limpiar datos, validar información o simplemente para una tarea específica, conocer cómo eliminar caracteres es una habilidad importante en el desarrollo de software.

## Cómo hacerlo

Para eliminar caracteres que coinciden con un patrón en C, podemos utilizar la función `strspn()` de la biblioteca estándar de C. Esta función toma dos argumentos: una cadena de caracteres a analizar y una cadena que contiene los caracteres a eliminar. A continuación, se muestra un ejemplo de cómo utilizar esta función:

```C
#include <stdio.h>
#include <string.h>

int main() {
	char texto[] = "Ejemplo123";
	char patrones[] = "0123456789"; // elimina todos los dígitos

	printf("Texto original: %s\n", texto); //  imprime "Ejemplo123"
	
	int cantidad_eliminada = strspn(texto, patrones); // devuelve la cantidad de caracteres eliminados
	texto[cantidad_eliminada] = '\0'; // añade el carácter nulo al final para indicar el nuevo final de la cadena
	
	printf("Texto sin dígitos: %s\n", texto); // imprime "Ejemplo"
}
```

Output:

```
Texto original: Ejemplo123
Texto sin dígitos: Ejemplo
``` 

## Profundizando

La función `strspn()` utiliza un enfoque de "barrido" para eliminar los caracteres que coinciden con el patrón. Comienza al principio de la cadena y va avanzando carácter por carácter hasta encontrar uno que no esté incluido en la cadena de patrones. Una vez que encuentra este carácter, devuelve la cantidad de caracteres que ha eliminado hasta ese punto. Luego, se utiliza esta cantidad para determinar el nuevo final de la cadena.

También hay otra función, `strcspn()`, que realiza el mismo proceso pero elimina los caracteres que no coinciden con el patrón. En combinación, estas dos funciones pueden ser muy útiles para limpiar y manipular cadenas de texto en C.

## Ver también

- Documentación oficial de `strspn()` en [C++ Reference](https://en.cppreference.com/w/c/string/byte/strspn)
- Artículo sobre la función `strcspn()` en [GeeksforGeeks](https://www.geeksforgeeks.org/c-strcspn-function/)