---
title:    "C: Leyendo un archivo de texto."
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/c/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Por qué

En muchas ocasiones como programadores, necesitamos leer y manipular archivos de texto en nuestro código. Esta habilidad es particularmente útil en la creación de aplicaciones de manejo de datos, análisis y muchas otras tareas. En este artículo, aprenderemos cómo leer un archivo de texto en C y cómo utilizar esta habilidad en nuestros proyectos.

## Cómo hacerlo

La lectura de un archivo de texto en C se puede hacer de varias maneras. Aquí presentamos un ejemplo simple utilizando la función `fopen()` y `fgetc()` para leer cada carácter del archivo y mostrarlo en la pantalla. Nota: Este ejemplo asume que el archivo de texto está en la misma carpeta que el código.

```
#include <stdio.h>

int main() {

   FILE *archivo;
   char caracter;

   archivo = fopen("archivo.txt", "rt");

   if (archivo == NULL) {
      printf("No se pudo abrir el archivo.\n");
      return 1;
   }

   while ((caracter = fgetc(archivo)) != EOF) {
      printf("%c", caracter);
   }

   fclose(archivo);

   return 0;
}
```

El resultado de este programa es la impresión del contenido del archivo de texto directamente en la pantalla.

## Profundizando

Para aquellos que deseen profundizar en la lectura de archivos de texto en C, aquí hay algunas consideraciones adicionales:

- La función `fopen()` puede recibir diferentes argumentos para especificar el modo de apertura del archivo (lectura, escritura, etc.)
- También es posible utilizar la función `fgets()` para leer una línea completa del archivo en lugar de un solo carácter.
- Es importante cerrar el archivo después de su uso mediante la función `fclose()`.

Con estas consideraciones y explorando más en la documentación de estas funciones, podrás empezar a utilizar la lectura de archivos de texto en C para tus necesidades específicas.

## Ver también

- Más información sobre `fopen()`: https://www.tutorialspoint.com/c_standard_library/c_function_fopen.htm
- Documentación detallada de `fgets()`: https://www.tutorialspoint.com/c_standard_library/c_function_fgets.htm
- Cómo manejar errores en lectura de archivos: https://www.tutorialspoint.com/c_standard_library/c_function_ferror.htm