---
title:                "C: Capitalizando una cadena"
programming_language: "C"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Por qué

A muchos desarrolladores les puede parecer una tarea trivial, pero capitalizar strings es esencial en muchos proyectos de programación. Ya sea para mejorar la presentación de datos en una interfaz de usuario o para procesar entradas de texto, saber cómo capitalizar una cadena de texto es una habilidad importante en cualquier lenguaje de programación. En este post, aprenderemos cómo capitalizar una cadena en C y profundizaremos en la lógica detrás de este proceso.

## Cómo hacerlo

Para capitalizar una cadena de texto en C, podemos seguir estos pasos:

1. Definir la cadena de texto original.
2. Crear una nueva cadena de texto vacía para guardar la versión capitalizada.
3. Inicializar un bucle para recorrer cada carácter de la cadena original.
4. Usar la función `toupper()` para convertir cada carácter en mayúscula y agregarlo a la nueva cadena.
5. Comprobar si el carácter actual es un espacio en blanco, si es así, se agrega un espacio en blanco a la nueva cadena.
6. Repetir hasta que se hayan procesado todos los caracteres de la cadena original.
7. Imprimir la nueva cadena capitalizada.

Veamos un ejemplo de código para capitalizar una cadena en C:

```C
#include <stdio.h>
#include <ctype.h> // Librería necesaria para la función toupper()

int main() {

   // Definición de la cadena original
   char str[] = "este es un ejemplo de cadena a capitalizar";

   // Nueva cadena para guardar la versión capitalizada
   char newStr[100];

   // Bucle para recorrer cada carácter de la cadena original
   int i;
   for(i = 0; str[i] != '\0'; i++) {

      // Convierte el carácter actual en mayúscula y lo agrega a la nueva cadena
      newStr[i] = toupper(str[i]);

      // Comprueba si el carácter actual es un espacio en blanco
      if(newStr[i] == ' ') {
         // Si es así, agrega un espacio en blanco a la nueva cadena
         newStr[i] = ' ';
      }
   }

   // Agrega el carácter de final de cadena a la nueva cadena
   newStr[i] = '\0';

   // Imprime la nueva cadena capitalizada
   printf("La cadena capitalizada es: %s", newStr);

   return 0;
}
```

El resultado de este código sería:

```
La cadena capitalizada es: ESTE ES UN EJEMPLO DE CADENA A CAPITALIZAR
```

## Profundizando

El proceso de capitalizar una cadena puede ser un poco más complejo de lo que parece. Por ejemplo, ¿qué sucede si la cadena original contiene palabras en mayúsculas o caracteres especiales? ¿Cómo podemos asegurarnos de que solo las letras se conviertan a mayúscula y no los espacios en blanco o signos de puntuación?

Una forma de mejorar nuestra lógica es utilizando una función auxiliar como `isalpha()` para comprobar si el carácter actual es una letra antes de convertirlo en mayúscula. También, podemos utilizar la función `tolower()` para convertir todos los caracteres a minúscula y luego volver a usar `toupper()` solo en las letras iniciales de cada palabra. De esta manera, evitamos convertir mayúsculas ya presentes en la cadena original.

Además, también podemos utilizar variables auxiliares para mantener un seguimiento de la posición de cada palabra y asegurarnos de que solo se capitalice la primera letra de cada palabra.

Existen muchas formas de mejorar y personalizar la lógica de capitalización de cadenas en C, y más allá de esta simple implementación, dependiendo de las necesidades de cada proyecto.

## Ver también

- [Ejemplos de capitalización de cadenas en C](https://www.geeksforgeeks.org/c-program-capitalizes-first-letter-string/)
- [Documentación de la función toupper() en C](https://www.tutorialspoint.com/c_standard_library/c_function_toupper.htm)
- [Ejemplo de código para capitalizar una cadena utilizando punteros en C](https://www.sanfoundry.com/c-program-replace-space-single-character/)
- [Implementación más avanzada de capitalizar una cadena en C](https://www.utf8-chartable.de/unicode-utf8-table.pl?start=8192&utf8=0x)