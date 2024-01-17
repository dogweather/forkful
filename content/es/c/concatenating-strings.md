---
title:                "Concatenando cadenas"
html_title:           "C: Concatenando cadenas"
simple_title:         "Concatenando cadenas"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c/concatenating-strings.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

La concatenación de cadenas es un término utilizado en programación para referirse a la unión de dos o más cadenas de texto en una sola. Los programadores realizan esta acción para combinar diferentes variables y crear una cadena de texto más larga y completa.

## Cómo:

El lenguaje C ofrece diferentes formas de concatenar cadenas. A continuación se presentan algunos ejemplos de cómo realizar esta acción:

```C
// Utilizando el operador de concatenación "+"
char saludo[] = "Hola ";
char nombre[] = "Juan";
char frase_saludo[12];

frase_saludo = saludo + nombre;
printf("%s", frase_saludo);
// Salida: "Hola Juan"

// Utilizando la función strcat()
char saludo[] = "Hola ";
char nombre[] = "Juan";
char frase_saludo[12];

strcat(frase_saludo, saludo);
strcat(frase_saludo, nombre);
printf("%s", frase_saludo);
// Salida: "Hola Juan"

// Utilizando sprintf()
char frase[20];

sprintf(frase, "Hola %s", "Juan");
printf("%s", frase);
// Salida: "Hola Juan"
```

## Profundizando:

La concatenación de cadenas es una técnica comúnmente utilizada en lenguajes de programación, especialmente en aquellos orientados a la manipulación de cadenas de texto. Antes de la introducción de funciones específicas como ```strcat()``` en C, los programadores debían implementar su propia lógica para concatenar cadenas.

Además de las formas mencionadas anteriormente, también es posible realizar concatenación utilizando punteros y la función ```strcpy()```. Es importante tener en cuenta que, al unir cadenas, se debe asegurar tener suficiente espacio para almacenar la cadena final resultante.

## Ver también:

- [Documentación oficial de C](https://devdocs.io/c/)
- [Ejemplos de concatenación de cadenas en C](https://www.programiz.com/c-programming/examples/concatenate-string)