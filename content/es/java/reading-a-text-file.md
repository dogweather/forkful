---
title:    "Java: Leyendo un archivo de texto"
keywords: ["Java"]
---

{{< edit_this_page >}}

# Por qué leer un archivo de texto en Java

Leer un archivo de texto en Java puede ser una tarea muy útil en la programación, ya que nos permite acceder y utilizar información almacenada en un formato legible para los humanos. En este artículo, aprenderemos cómo leer un archivo de texto en Java y exploraremos algunos aspectos más profundos de esta técnica.

## Cómo hacerlo

Para leer un archivo de texto en Java, primero debemos importar la clase `File` de la librería `java.io`. Esta clase nos permite crear una instancia de un archivo y manipularlo. Luego, utilizamos la clase `Scanner` para leer el contenido del archivo línea por línea.

```Java
import java.io.File;
import java.util.Scanner;

public class LeerArchivoTexto {
    public static void main(String[] args) {
        // Creamos una instancia del archivo que queremos leer
        File archivo = new File("ejemplo.txt");
        // Creamos una instancia de Scanner para leer el archivo
        Scanner scanner = new Scanner(archivo);

        // Iteramos sobre cada línea del archivo e imprimimos su contenido
        while (scanner.hasNextLine()) {
            String linea = scanner.nextLine();
            System.out.println(linea);
        }
        // Cerramos el Scanner
        scanner.close();
    }
}
```

Si nuestro archivo de texto contiene el siguiente contenido:

```
Hola mundo!
Este es un ejemplo de un archivo de texto.
```

La salida del código anterior sería:

```
Hola mundo!
Este es un ejemplo de un archivo de texto.
```

## Profundizando más

Además de leer y mostrar el contenido de un archivo de texto, también podemos realizar otras operaciones, como por ejemplo contar la cantidad de líneas o palabras, buscar una determinada cadena de texto, o incluso escribir en el archivo. Para realizar estas tareas, es necesario utilizar otras clases de la librería `java.io` como `FileReader` o `BufferedReader`.

Otra opción interesante es utilizar la clase `FileWriter` o `BufferedWriter` para escribir en un archivo de texto. De esta manera, podemos crear o modificar un archivo con nuestro propio código en Java.

## Ver también

- [Documentación oficial de Java sobre archivos de texto](https://docs.oracle.com/javase/tutorial/essential/io/file.html)
- [Tutorial de lectura de archivos en Java](https://www.geeksforgeeks.org/file-handling-java-using-scanner/)
- [Tutorial de escritura de archivos en Java](https://www.javatpoint.com/java-filewriter-class)