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

¿Qué y por qué los programadores extraen subcadenas (substrings)?

Extraer subcadenas (substrings) es un proceso común en el desarrollo de programas en C. Básicamente, se trata de seleccionar una parte de una cadena de texto más grande para usarla en otra parte del programa. Los programadores pueden hacerlo para diferentes propósitos, como manipular datos, buscar patrones o simplemente simplificar el código.

¿Cómo hacerlo?

Para extraer subcadenas en C, se pueden usar las funciones ```strncpy()``` y ```strstr()```. La primera copia una cantidad específica de caracteres de una cadena a otra, mientras que la segunda busca una subcadena dentro de una cadena más grande. A continuación se muestra un ejemplo de cómo usar estas funciones:

```C
char cadena[] = "¡Hola, mundo!";
char subcadena[5];
int tamaño = 4;
strncpy(subcadena, cadena, tamaño);
printf("Subcadena: %s", subcadena);
```

Esto imprimirá "Hola" ya que ```strncpy()``` toma los primeros cuatro caracteres de la cadena original y los copia en la nueva subcadena. También se puede usar ```strstr()``` para buscar una subcadena específica dentro de otra, como en el siguiente ejemplo:

```C
char cadena[] = "¡Hola, mundo!";
char subcadena[] = "Mundo";
char* resultado = strstr(cadena, subcadena);
printf("Subcadena encontrada: %s", resultado);
```

Esto imprimirá "Mundo" ya que ```strstr()``` encuentra la subcadena dentro de la cadena original y la devuelve como un puntero.

Profundizando más

La idea de extraer subcadenas no es nueva, de hecho, algunas personas clasifican las funciones ```strncpy()``` y ```strstr()``` como obsoletas debido a su complejidad de uso. Alternativas a considerar son la función ```strncat()```, que concatena dos cadenas y especifica una longitud máxima, y la biblioteca "string.h" que contiene funciones útiles para manipular cadenas de texto.

En cuanto a la implementación de estas funciones, la mayoría de los compiladores de C tienen optimizaciones que pueden acelerar el proceso de extracción de subcadenas. Por ejemplo, algunos usan "Instrucciones SIMD" para realizar múltiples operaciones en una sola instrucción, lo que puede mejorar significativamente el rendimiento.

Para saber más sobre extracción de subcadenas en C, se pueden consultar estos enlaces:

- [Documentación oficial de C](https://devdocs.io/c/)
- [Explicación detallada de funciones de cadenas en C](https://c-for-dummies.com/blog/?p=1113)
- [Alternativas a las funciones ```strncpy()``` y ```strstr()```](https://stackoverflow.com/questions/54222956/alternative-functions-to-strncpy-strstr-strcmp-inc-c-string)

¡Ahora ya sabes cómo extraer subcadenas en C! Esperamos que estas funciones te ayuden a simplificar tu código y a ser más eficiente en tu programación. ¡Hasta la próxima!