---
title:                "Eliminando caracteres que coinciden con un patrón"
html_title:           "Elixir: Eliminando caracteres que coinciden con un patrón"
simple_title:         "Eliminando caracteres que coinciden con un patrón"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# "## ¿Qué y Por Qué?"
La eliminación de caracteres que coinciden con un patrón es una técnica usada para depurar cadenas en la programación de C. Los programadores hacen esto para filtrar o hacer una limpieza consistente de datos, mejorando la eficiencia y la facilidad de manejo.

# "## ¿Cómo hacerlo?:"
Vamos a aprender a eliminar todos los caracteres en una cadena que coinciden con un cierto patrón. Aquí está el código base:

```C
#include <stdio.h>
#include <string.h>

void eliminarChars(char *s, char c) { 
   int j, n = strlen(s); 
   for (int i=j=0; i<n; i++) 
      if (s[i] != c) 
         s[j++] = s[i]; 
      
   s[j] = '\0'; 
}

int main() { 
   
   char s[100]; 
   fgets(s, sizeof(s), stdin);

   // Eliminate 'a'
   eliminarChars(s, 'a'); 
   
   printf("%s", s);

   return 0; 
}

```

Cuando ejecutas el programa anterior, si introduces la frase "hola como estas", la salida será "hol como ests", ya que hemos eliminado todas las 'a'.

# "## Análisis detallado:"
La eliminación de caracteres se lleva a cabo desde los primeros días del lenguaje C para manipular cadenas de caracteres de manera más efectiva. Existe una función más sofisticada llamada strpbrk() en la biblioteca de C para encontrar la primera ocurrencia de un conjunto de caracteres, pero para la simple eliminación de caracteres, 'for' y 'if' son suficientes.

Los programadores han implementado esta técnica de manera diferente en función de sus necesidades. Algunos pueden preferir funciones reutilizables en una biblioteca personalizada, mientras que otros pueden encontrar más rápido ejecutar sentencias condicionales en el lugar donde sea necesario.

# "## Ver también:"
1. "Manipulación de strings en C": Aquí encontrarás detalles de las diferentes formas en que puedes jugar con las cadenas en C.
2. "Biblioteca de funciones de C": Visite este enlace para obtener información detallada sobre la biblioteca de funciones de C y cómo puede facilitar la programación en C.