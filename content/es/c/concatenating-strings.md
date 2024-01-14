---
title:                "C: Uniendo cadenas de texto"
programming_language: "C"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c/concatenating-strings.md"
---

{{< edit_this_page >}}

## Por qué
En programación, a menudo necesitamos combinar varias cadenas de texto juntas para formar una sola cadena más grande. Esto se conoce como concatenación de cadenas y es una habilidad esencial para cualquier programador en C. Al dominar la concatenación de cadenas, podrás crear mensajes personalizados, formularios dinámicos y mucho más.

## Cómo hacerlo
En C, podemos concatenar cadenas utilizando la función `strcat()`. Esta función toma dos cadenas como argumentos y une la segunda cadena al final de la primera. Aquí hay un ejemplo de cómo usarlo:

```C
#include <stdio.h>
#include <string.h>

int main() {
    char nombre[] = "Juan";
    char apellido[] = "Perez";
    char nombre_completo[50];
    strcpy(nombre_completo, nombre); // Copiamos 'nombre' a 'nombre_completo'
    strcat(nombre_completo, " "); // Añadimos un espacio en blanco
    strcat(nombre_completo, apellido); // Concatenamos 'apellido'
    printf("Tu nombre completo es: %s", nombre_completo); // Imprimimos la cadena resultante
    return 0;
}
```
**Output:**
```
Tu nombre completo es: Juan Perez
```

¡Fácil, verdad? También podemos usar la función `strncat()` para concatenar una cierta cantidad de caracteres de la segunda cadena. Consulta la [documentación oficial](http://www.cplusplus.com/reference/cstring/strcat/) para más detalles y opciones.

## Profundizando
Es importante tener en cuenta que a diferencia de otros lenguajes de programación, en C no existe una función integrada para concatenar cadenas. Por lo tanto, debemos utilizar funciones como `strcat()` y `strcpy()` para lograrlo. Además, es importante asegurarse de que la cadena resultante tenga suficiente espacio para contener ambas cadenas. Si no, pueden ocurrir errores inesperados.

Otro aspecto importante es que las cadenas en C son en realidad arreglos de caracteres, por lo que la concatenación realmente implica la manipulación de estos arreglos. Esto significa que debes prestar atención a la dirección de memoria en la que estás concatenando y asegurarte de no sobrescribir otros datos importantes.

Por último, pero no menos importante, es importante tener en cuenta el rendimiento al concatenar cadenas en C. Si necesitas hacer muchas concatenaciones en un programa, puede ser más eficiente usar una biblioteca de cadenas como `string.h` y sus funciones integradas como `sprintf()`.

## Ver también
- [Documentación oficial de strcat()](http://www.cplusplus.com/reference/cstring/strcat/)
- [Otras funciones de cadena en C](http://www.cplusplus.com/reference/cstring/)
- [Cómo evitar errores comunes al concatenar cadenas en C](https://stackify.com/common-c-string-functions/)