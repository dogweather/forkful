---
title:                "Leyendo un archivo de texto."
html_title:           "Java: Leyendo un archivo de texto."
simple_title:         "Leyendo un archivo de texto."
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/java/reading-a-text-file.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

Leer un archivo de texto es una tarea común en la programación, ya que permite a los programadores almacenar y manejar grandes cantidades de datos de manera eficiente. Los archivos de texto son una forma sencilla y legible para almacenar información en un formato que puede ser interpretado por una computadora.

## ¿Cómo hacerlo?

La lectura de un archivo de texto en Java es un proceso sencillo que se puede realizar utilizando la clase `Scanner`. Primero, debes asegurarte de tener la ruta del archivo en la que se encuentra el archivo de texto. Luego, puedes utilizar el método `nextLine()` de `Scanner` para leer cada línea del archivo y almacenarla en una variable.

```Java
import java.io.*;
import java.util.Scanner;

public class FileReader {
    public static void main(String[] args) {
        // Ruta del archivo
        String filePath = "miArchivo.txt";
        
        try {
            // Creamos un objeto Scanner para leer el archivo
            Scanner scanner = new Scanner(new File(filePath));
            
            // Iteramos a través de cada línea del archivo
            while(scanner.hasNextLine()){
                // Almacenamos la línea en una variable
                String line = scanner.nextLine();
                // Imprimimos la línea en la consola
                System.out.println(line);
            }
            
            // Cerramos el Scanner
            scanner.close();
            
        } catch (FileNotFoundException e) {
            System.out.println("Archivo no encontrado: " + e.getMessage());
        }
    }
}
```

El código anterior imprimirá cada línea del archivo de texto en la consola. Si quieres almacenar la información en una estructura de datos, puedes utilizar un ArrayList o un HashMap para almacenar cada línea como un elemento y acceder a ella posteriormente.

## Profundizando

En el pasado, los archivos de texto eran la única forma de almacenar y compartir información entre diferentes computadoras. Con el avance de la tecnología, han surgido otros formatos de archivo, como las bases de datos, que se han vuelto más populares debido a su eficiencia y funcionalidades avanzadas.

Además de la clase `Scanner`, también se pueden utilizar otras librerías como `BufferedReader` o `FileInputStream` para leer archivos de texto en Java. Sin embargo, la clase `Scanner` se considera la opción más sencilla y eficiente para esta tarea.

## Ver también

- [Documentación oficial de Java sobre la clase Scanner](https://docs.oracle.com/javase/10/docs/api/java/util/Scanner.html)
- [Tutorial de Java sobre la lectura de archivos de texto](https://www.javatpoint.com/java-read-file)
- [Diferentes formas de leer archivos en Java](https://www.geeksforgeeks.org/different-ways-reading-text-file-java/)