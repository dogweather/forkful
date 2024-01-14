---
title:                "C: Escribiendo un archivo de texto"
simple_title:         "Escribiendo un archivo de texto"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c/writing-a-text-file.md"
---

{{< edit_this_page >}}

##Por qué escribir un archivo de texto en C

Escribir un archivo de texto en C puede ser una herramienta útil para guardar información importante o resultados de un programa. Además, puede ayudar a hacer que tus programas sean más versátiles y fáciles de compartir con otras personas.

##Cómo hacerlo

Para escribir un archivo de texto en C, primero debes abrir el archivo utilizando la función `fopen()`. Luego, puedes escribir en el archivo utilizando la función `fprintf()` y para finalizar, debes cerrar el archivo con la función `fclose()`. Aquí hay un ejemplo de cómo escribir "¡Hola mundo!" en un archivo de texto llamado "ejemplo.txt":

```
#include<stdio.h>

int main()
{
    FILE *archivo = fopen("ejemplo.txt", "w");
    fprintf(archivo, "¡Hola mundo!");
    fclose(archivo);
    return 0;
}
```

Si ejecutamos este programa, se creará un archivo de texto llamado "ejemplo.txt" con el texto "¡Hola mundo!" dentro de él.

##Profundizando

Cuando usas la función `fopen()`, puedes especificar el modo en el cual quieres abrir el archivo. En el ejemplo anterior, usamos "w" como modo, que significa que el archivo se abrirá para escritura y se creará si no existe. Pero también hay otros modos disponibles, como "r" para lectura o "a" para añadir contenido al final del archivo. Puedes consultar la documentación de la función `fopen()` para conocer más sobre los diferentes modos disponibles y sus usos.

Además, también es importante tener en cuenta que si el archivo ya existe y lo abres en modo de escritura, todo su contenido anterior se sobrescribirá. Si deseas preservar el contenido anterior del archivo, debes abrirlo en modo de añadir ("a") en lugar de modo de escritura ("w").

##Ver también

- Documentación de `fopen()`: https://www.tutorialspoint.com/c_standard_library/c_function_fopen.htm
- Documentación de `fprintf()`: https://www.tutorialspoint.com/c_standard_library/c_function_fprintf.htm
- Documentación de `fclose()`: https://www.tutorialspoint.com/c_standard_library/c_function_fclose.htm