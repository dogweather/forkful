---
title:                "C: Extrayendo subcadenas"
programming_language: "C"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c/extracting-substrings.md"
---

{{< edit_this_page >}}

## Por qué extraer subcadenas en programación?

Extraer subcadenas es una técnica útil en programación cuando necesitamos obtener una parte específica de una cadena de texto más grande. Esto puede ser útil para manipular datos, realizar búsquedas más precisas o simplemente para obtener información específica de una cadena de texto.

## Cómo hacerlo

Para extraer subcadenas en C, podemos utilizar la función `strcpy()` y la función `strncpy()`. La función `strcpy()` copia una cadena de texto a otra, mientras que la función `strncpy()` copia una cantidad específica de caracteres de una cadena de texto a otra. Ambas funciones requieren dos parámetros: el destino y el origen. Por ejemplo:

```
char source[] = "Hola Mundo";
char destination[6];
strcpy(destination, source); // destination es ahora "Hola "
```

En este ejemplo, la función `strcpy()` copió los primeros seis caracteres de la cadena de texto `source` en la cadena de texto `destination`.

También podemos utilizar la función `substr()` de la biblioteca `string.h` para extraer una subcadena a partir de un índice específico. Por ejemplo:

```
char source[] = "Hola Mundo";
char destination[11];
substr(destination, source, 5, 5); // destination es ahora "Mundo"
```

En este ejemplo, la función `substr()` copió los cinco caracteres a partir del índice 5 de la cadena de texto `source` en la cadena de texto `destination`.

## Detalles sobre la extracción de subcadenas

Al utilizar la función `strncpy()`, debemos tener en cuenta que si la cantidad de caracteres que estamos copiando es mayor que la longitud del destino, no se agregará un carácter nulo al final de la cadena. Por lo tanto, es importante seguir utilizando la función `strcpy()` para asegurarnos de que nuestra subcadena tenga el carácter nulo al final para evitar errores.

Además, al utilizar la función `substr()`, debemos tener en cuenta que el índice de inicio en realidad es el índice de la posición del primer carácter que queremos copiar. Por lo tanto, si queremos copiar los primeros cinco caracteres de una cadena de texto, el índice de inicio debería ser 0 en lugar de 1.

## Ver también

- [Documentación de la función strcpy() en C (en español)](https://www.it-swarm.dev/es/c/strncpy-copia-error-de-una-cadena/1048664717/)
- [Documentación de la función strncpy() en C (en español)](https://www.lawebdelprogramador.com/codigo.cfm?id=1420)
- [Tutorial sobre extracción de subcadenas en C (en español)](https://www.youtube.com/watch?v=ZEFoKGEo5c0)