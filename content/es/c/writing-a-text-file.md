---
title:    "C: Redactar un archivo de texto"
keywords: ["C"]
---

{{< edit_this_page >}}

## Por qué

¿Alguna vez has querido guardar información en un archivo de texto para leerlo más tarde? ¡Escribir un archivo de texto en C es una gran opción! Con esta habilidad, podrás guardar y acceder a información importante en cualquier momento.

## Cómo hacerlo

Primero, debes entender que un archivo de texto es simplemente una secuencia de caracteres almacenada en un archivo. Nuestro objetivo es escribir nuestros propios caracteres en un archivo y luego acceder a ellos más tarde.

Para hacer esto, utilizaremos la función `fopen()` para abrir un nuevo archivo y especificar el modo en el que queremos escribir en él. Luego, utilizaremos la función `fprintf()` para escribir los caracteres que queramos en el archivo. Finalmente, usaremos la función `fclose()` para cerrar el archivo y asegurarnos de que todo se haya guardado correctamente.

Veamos un ejemplo de código para escribir un archivo de texto en C:

```C
#include <stdio.h>
int main(){
    
    //Abrimos el archivo "miarchivo.txt" para escribir en él
    FILE *fp = fopen("miarchivo.txt", "w");
    
    //Escribimos "Hola mundo" en el archivo
    fprintf(fp, "Hola mundo");
    
    //Cerramos el archivo
    fclose(fp);
    
    return 0;
}
```

Si ejecutamos este programa, veremos que se creó un archivo llamado "miarchivo.txt" y que contiene la frase "Hola mundo". ¡Felicidades, acabas de escribir tu primer archivo de texto en C!

## Profundizando

Ahora que sabes cómo escribir un archivo de texto, es importante entender cómo manejar posibles errores que puedan surgir. Por ejemplo, si el archivo que estamos tratando de abrir no existe, la función `fopen()` devolverá NULL, lo que indica un error. También es importante recordar cerrar el archivo con `fclose()` para asegurarse de que todo se haya guardado correctamente.

También podemos utilizar la función `fputs()` para escribir una cadena de caracteres en un archivo. Esta función nos permite especificar el tamaño máximo de la cadena y también devuelve un valor para indicar si la escritura fue exitosa o no.

Otra forma de escribir en un archivo es utilizando la función `fwrite()`, que nos permite especificar el tamaño de cada elemento que queremos escribir y el número total de elementos. Esta función es útil cuando queremos escribir datos más complejos en un archivo.

## Ver también

- [Cómo leer un archivo de texto en C](enlace1)
- [Más información sobre la función fopen()](enlace2)
- [Documentación de la función fprintf()](enlace3)