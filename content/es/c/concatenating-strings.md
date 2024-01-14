---
title:    "C: Uniendo cadenas de texto"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/c/concatenating-strings.md"
---

{{< edit_this_page >}}

## Por qué: 

Concatenar strings es una técnica fundamental en la programación que permite combinar múltiples cadenas de texto en una sola. Es especialmente útil en aquellos casos en los que se necesite crear mensajes personalizados o manipular datos de manera eficiente.

## Cómo hacerlo:

La concatenación de strings en C es posible gracias a la función `strcat()`, que forma parte de la biblioteca estándar `string.h`. Esta función recibe dos parámetros: la cadena de destino y la cadena que se desea añadir a la primera. A continuación, se muestra un ejemplo de cómo usar `strcat()`:

```C
#include <stdio.h>
#include <string.h> // biblioteca de strings

int main() {
    char nombre[20] = "Juan"; // cadena de destino
    char apellido[20] = "Pérez"; // cadena que se desea añadir

    strcat(nombre, " "); // se agrega un espacio en blanco
    strcat(nombre, apellido); // se añade el apellido a la cadena de destino
    
    printf("Mi nombre completo es %s.\n", nombre);
    
    return 0;
}
```

La salida de este código sería: `Mi nombre completo es Juan Pérez.`

## Profundizando:

Es importante tener en cuenta que para usar `strcat()`, la cadena de destino debe tener suficiente espacio para almacenar la cadena que se desea añadir. De lo contrario, podría producirse un desbordamiento de memoria y generar errores en el programa. Además, esta función solo puede ser utilizada con datos de tipo `char`, por lo que si se desea concatenar otros tipos de datos, es necesario convertirlos previamente a `char`.

Otra función útil para concatenar strings es `sprintf()`, que permite crear una cadena de texto con formato a partir de distintas variables. A continuación, se muestra un ejemplo de su uso:

```C
#include <stdio.h>

int main() {
    char saludo[50];

    int edad = 25;
    float altura = 1.80;
    
    // se crea la cadena de texto con formato
    sprintf(saludo, "Hola, tengo %d años y mido %.2f metros.", edad, altura);
    
    printf("%s\n", saludo);
    
    return 0;
}
```

La salida de este código sería: `Hola, tengo 25 años y mido 1.80 metros.`

## Ver también:

- [Tutorial de la función strcat()](https://www.codementor.io/@francleidefonse/study-tutorial-to-concatenate-two-strings-in-c-language-2edz5zm6a)
- [Referencia de la función sprintf()](https://www.programiz.com/c-programming/library-function/stdio.h/sprintf)