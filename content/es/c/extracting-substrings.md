---
title:                "Extrayendo subcadenas"
html_title:           "C: Extrayendo subcadenas"
simple_title:         "Extrayendo subcadenas"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c/extracting-substrings.md"
---

{{< edit_this_page >}}

## ¿Por qué deberías aprender a extraer subcadenas?

Extraer subcadenas es una habilidad fundamental en la programación ya que te permite manipular y trabajar con cadenas de caracteres de manera más eficiente. Al conocer cómo extraer subcadenas, podrás realizar tareas como buscar y reemplazar dentro de una cadena, validar datos de entrada y generar resultados específicos basados en ciertas partes de una cadena.

## Cómo hacerlo en C

Para extraer subcadenas en C, necesitamos utilizar la función `strncpy()`. Esta función toma tres argumentos: la cadena de destino, la cadena de origen y la cantidad de caracteres a copiar de la cadena de origen a la cadena de destino. Aquí hay un ejemplo de cómo utilizar `strncpy()` para extraer una subcadena:

```C
#include <stdio.h>
#include <string.h>

int main() {
    char nombre[20] = "Juan Perez";
    // Extraemos la subcadena "Perez"
    char apellido[20];
    // Utilizamos strncpy para copiar 5 caracteres de "Perez" a la cadena apellido
    strncpy(apellido, nombre + 5, 5);
    // Agregamos un carácter nulo al final de la subcadena
    apellido[5] = '\0';
    // Imprimimos la subcadena
    printf("El apellido es: %s\n", apellido);
    
    return 0;
}

// Output: El apellido es: Perez
```

## En profundidad sobre la extracción de subcadenas

Ahora que sabes cómo utilizar `strncpy()` para extraer subcadenas, es importante entender cómo funciona exactamente esta función. `strncpy()` copia la cantidad especificada de caracteres de la cadena de origen (el segundo argumento) a la cadena de destino (el primer argumento). Sin embargo, si la cadena de origen es más larga que la cantidad especificada, no se agregará un carácter nulo al final de la cadena de destino. Esto puede causar problemas si, por ejemplo, intentas imprimir la cadena de destino como si fuera una cadena completa.

Para evitar este problema, es importante siempre agregar un carácter nulo al final de la subcadena después de copiar los caracteres necesarios. En el ejemplo anterior, utilizamos `apellido[5] = '\0'` para agregar un carácter nulo al final de la subcadena extraída.

## Ver también

Aquí te dejamos algunos enlaces útiles para que puedas profundizar en el tema de la extracción de subcadenas en C:

- [Documentación oficial de `strncpy()` en cplusplus.com](http://www.cplusplus.com/reference/cstring/strncpy/)
- [Tutorial sobre cadenas en C en SoloLearn](https://www.sololearn.com/Course/C)
- [Más ejemplos de cómo trabajar con cadenas en C en Tutorialspoint](https://www.tutorialspoint.com/cprogramming/c_strings.htm)