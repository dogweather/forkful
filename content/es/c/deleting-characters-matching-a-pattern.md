---
title:    "C: Borrando caracteres que coinciden con un patrón"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/c/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## ¿Por qué borrar caracteres que coinciden con un patrón?

Hay muchas razones por las que un programador podría querer borrar caracteres que coinciden con un patrón en su código en C. Puede ser para mejorar la legibilidad del código, para eliminar caracteres innecesarios o para cumplir con ciertos requisitos de formato. En cualquier caso, saber cómo realizar esta tarea en C puede ser muy útil en muchas situaciones.

## Cómo hacerlo

Borrar caracteres que coinciden con un patrón en C es sencillo y se puede lograr utilizando la función de biblioteca estándar ```strstr()```, que busca una subcadena dentro de una cadena dada. Primero, necesitamos importar la biblioteca ```<string.h>``` para acceder a esta función. Luego, podemos usar la función ```strlen()``` para obtener la longitud de la cadena original y luego usar un bucle for para recorrer todos los caracteres y compararlos con nuestro patrón utilizando ```strstr()```. Si se encuentra un patrón coincidente, simplemente podemos usar la función ```memmove()``` para sobrescribir el patrón con el siguiente carácter en la cadena. Este proceso se repetirá hasta que se hayan eliminado todas las coincidencias del patrón en la cadena original.

A continuación se muestra un ejemplo de cómo implementar esto en código C:

```C
#include <stdio.h>
#include <string.h>

int main() {
  // Ejemplo de una cadena con caracteres innecesarios
  char cadena[] = "H0l4 M4! @#$";
  // Patrón a borrar
  char patron[] = "@#$";
  // Obtener la longitud de la cadena original
  int longitud = strlen(cadena);
  // Variables para comparar y sobrescribir
  char *posicion;
  int i;
  // Bucle for para recorrer la cadena
  for (i = 0; i < longitud; i++) {
    // Buscar patrón utilizando la función strstr()
    posicion = strstr(cadena, patron);
    // Si se encuentra una coincidencia
    if (posicion != NULL) {
      // Sobrescribir el patrón con el siguiente carácter
      memmove(posicion, posicion + 1, strlen(posicion));
    }
  }
  // Imprimir la cadena resultante sin el patrón
  printf("Cadena resultante: %s", cadena);
  return 0;
}
```

La salida de este código sería:

```
Cadena resultante: H0l4 M4! 
```

## Profundizando

La función ```strstr()``` se utiliza comúnmente para buscar patrones en cadenas en C. También es importante mencionar que esta función distingue mayúsculas y minúsculas, por lo que si se desea una búsqueda sensible a mayúsculas y minúsculas, se puede utilizar la función ```strcasestr()``, que se encuentra en la biblioteca ```<strings.h>```. 

Otra manera de borrar caracteres que coinciden con un patrón es utilizando la función ```strtok()```, que divide una cadena en tokens basados en un carácter delimitador. Podríamos usar esta función para dividir la cadena original en tokens y luego eliminar los tokens que contienen nuestro patrón. Esta podría ser una forma más eficiente de eliminar múltiples instancias de un patrón en una cadena.

## Consulta también

- [Función strstr() en C](https://www.geeksforgeeks.org/strstr-in-c/)
- [Función memmove() en C](https://www.geeksforgeeks.org/memmove-in-c/)
- [Función strtok() en C](https://www.geeksforgeeks.org/strtok-strtok_r-functions-c-examples/)