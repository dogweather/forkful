---
title:    "C: Escribiendo en el error estándar"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/c/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Por qué escribir al error estándar en C

Escribir al error estándar, también conocido como stderr en C, es una práctica común en la programación en este lenguaje. Esto se debe a que nos permite mostrar mensajes de error al usuario del programa, informándole sobre posibles fallos o errores en la ejecución del código.

## Cómo hacerlo

Para escribir al error estándar en C, utilizamos la función `fprintf()` y especificamos `stderr` como el primer parámetro. Por ejemplo:

```C
fprintf(stderr, "¡Ups! Algo salió mal.");
```
Esta línea de código imprimirá "¡Ups! Algo salió mal." en la salida de error del programa.

También podemos utilizar `perror()` para imprimir un mensaje de error seguido de la descripción del error en `errno`. Por ejemplo:

```C
perror("Error al abrir el archivo");
```

## Profundizando

La razón principal para escribir al error estándar en C es para proporcionar al usuario información sobre fallos o errores en la ejecución del código. Esta información puede ser útil para solucionar problemas y depurar el código.

Es importante destacar que la salida de error es diferente de la salida estándar (stdout) en el sentido de que la salida de error no se ve afectada por la redirección de la salida. Esto significa que incluso si se redirecciona la salida estándar a un archivo, los mensajes de error se seguirán imprimiendo en la pantalla.

Otra razón para escribir al error estándar es que nos permite distinguir entre mensajes informativos y mensajes de error. Si utilizamos `printf()` para imprimir mensajes de error, no podríamos diferenciarlos fácilmente de otros mensajes.

## Ver también

- [Referencia de la función `fprintf()` en C](https://www.tutorialspoint.com/c_standard_library/c_function_fprintf.htm)
- [Referencia de la función `perror()` en C](https://www.tutorialspoint.com/c_standard_library/c_function_perror.htm)