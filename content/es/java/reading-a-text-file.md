---
title:    "Java: Leyendo un archivo de texto."
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/java/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Why
Muchas veces, cuando escribimos código en Java, necesitamos acceder y leer archivos de texto. Esto puede ser para leer datos guardados en un archivo, crear informes basados en esos datos o simplemente para leer un archivo de configuración. Sin saber cómo leer un archivo de texto, puede ser difícil completar estas tareas con éxito.

## How To
En Java, leer un archivo de texto es un proceso sencillo que implica tres pasos: abrir el archivo, leer su contenido y cerrarlo. Veamos un ejemplo de cómo hacerlo:

```Java
// importar la clase File y la clase Scanner
import java.io.File;
import java.util.Scanner;

public class LeerArchivoTexto {
  public static void main(String[] args) {
    try {
      // crear un objeto File con la ruta del archivo
      File archivo = new File("ruta/del/archivo.txt");
      
      // crear un objeto Scanner y leer el archivo línea por línea
      Scanner scanner = new Scanner(archivo);
      while (scanner.hasNextLine()) {
        String linea = scanner.nextLine();
        System.out.println(linea);
      }
      
      // cerrar el objeto Scanner
      scanner.close();
    } catch (Exception e) {
      e.printStackTrace();
    }
  }
}
```

Si el archivo que estamos leyendo tiene el siguiente contenido:

```
Hola mundo
Mi nombre es Juan
Tengo 25 años
```

Entonces la salida del programa sería:

```
Hola mundo
Mi nombre es Juan
Tengo 25 años
```

## Deep Dive
Es importante tener en cuenta que al leer archivos de texto en Java, debemos manejar algunas excepciones en caso de que haya problemas al abrir el archivo o si el archivo no existe. Además, podemos utilizar otras clases como FileReader y BufferedReader para leer archivos de texto de diferentes maneras.

También es importante mencionar que en Java 8 se introdujo una nueva manera de leer archivos llamada Files.lines(). Esta función devuelve un Stream de líneas del archivo que podemos manipular utilizando expresiones lambda y las funciones de Stream de Java.

## Ver También
- [Clase File en Java](https://docs.oracle.com/javase/8/docs/api/java/io/File.html)
- [Clase Scanner en Java](https://docs.oracle.com/javase/8/docs/api/java/util/Scanner.html)
- [Clase FileReader en Java](https://docs.oracle.com/javase/8/docs/api/java/io/FileReader.html)
- [Clase BufferedReader en Java](https://docs.oracle.com/javase/8/docs/api/java/io/BufferedReader.html)
- [Stream de Java](https://docs.oracle.com/javase/8/docs/api/java/util/stream/Stream.html)

¡Ahora estás listo para leer y manipular archivos de texto en Java! Con este conocimiento, podrás realizar muchas tareas útiles, como manejar datos, configuraciones y generar informes en tus aplicaciones Java. ¡Sigue explorando las diferentes formas de leer archivos y desarrolla tus habilidades de programación en Java!