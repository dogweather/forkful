---
title:    "C: Uniendo cadenas de texto"
keywords: ["C"]
---

{{< edit_this_page >}}

## Por qué

La concatenación de cadenas es una habilidad fundamental para los programadores de C. Permite combinar varias cadenas de texto en una sola, lo que puede ser útil en muchas situaciones, como la creación de mensajes personalizados o la generación de reportes de datos.

## Cómo hacerlo

Para concatenar cadenas en C, utilizamos la función `strcat`. Esta función toma dos parámetros: la primera cadena y la segunda cadena que se desea agregar a la primera. Por ejemplo:

```C
char string1[50] = "Hola ";
char string2[] = "mundo!";
strcat(string1, string2);
printf("%s", string1);
```
**Output: Hola mundo!**

También podemos usar la función `sprintf` para concatenar cadenas, que nos permite especificar el formato de salida y agregar múltiples variables en una sola cadena. A continuación, un ejemplo:

```C
char nombre[20] = "Juan";
int edad = 25;
char frase[50];
sprintf(frase, "Mi nombre es %s y tengo %d años.", nombre, edad);
printf("%s", frase);
```
**Output: Mi nombre es Juan y tengo 25 años.**

## Profundizando

Es importante tener en cuenta que la función `strcat` agrega la segunda cadena a la primera y reemplaza el caracter nulo (`\0`) al final de la primera cadena con el segundo caracter nulo de la segunda cadena. Por eso es importante asegurarse de que la primera cadena tenga suficiente espacio para agregar la segunda.

Además, en caso de que las cadenas contengan caracteres especiales como `\n` o `\t`, la función `sprintf` los interpretará y los concatenará correctamente.

Por último, existen otras funciones útiles para manipular y concatenar cadenas, como `strcpy` para copiar cadenas y `strtok` para dividir una cadena en palabras o tokens.

## Ver también

- [Documentación de la función `strcat` en la biblioteca estándar de C](https://www.codingame.com/playgrounds/14213/how-to-concatenate-strings-in-c/strcat)
- [Ejemplos adicionales de concatenación de cadenas en C](https://www.codeforwin.org/2015/05/concatenate-two-strings-c-programming.html)
- [Tutorial en español sobre cadenas en C](https://prognotes.net/2015/05/c-tutorial-cadenas-de-caracteres/)