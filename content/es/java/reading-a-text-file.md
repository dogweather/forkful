---
title:                "Leyendo un archivo de texto"
html_title:           "Arduino: Leyendo un archivo de texto"
simple_title:         "Leyendo un archivo de texto"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/java/reading-a-text-file.md"
---

{{< edit_this_page >}}

## ¿Qué es y Por Qué?

Leer un archivo de texto implica obtener información almacenada en un archivo de texto. Los programadores lo hacen para acceder a datos guardados, procesarlos y usarlos en sus programas, como entrada de datos.

## Cómo hacerlo:

Para leer un archivo de texto en Java, usamos la clase `BufferedReader` de la biblioteca estándar de Java.

```Java
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

public class Main{
    public static void main (String[] args){
        try{
           BufferedReader reader = new BufferedReader(new FileReader("archivo.txt"));
           String linea;
           while ((linea = reader.readLine()) != null){
               System.out.println(linea);
           }
           reader.close();
        }catch (IOException e){
           e.printStackTrace();
        }
    }
}
```
La salida será el contenido de su archivo de texto.

## Inmersión Profunda

En los primeros días, los programadores solían manejar cada archivo de texto byte por byte, pero gracias a las bibliotecas modernas como `BufferedReader` en Java, leemos los archivos de texto más eficientemente.

Entre las alternativas a `BufferedReader` están `Scanner` y `FileReader`.

Aunque `Scanner` es fácil de usar, `BufferedReader` es mucho más eficiente para leer grandes cantidades de datos. `FileReader` se utiliza para leer caracteres y es menos eficiente que las dos opciones anteriores.

Por cada método de lectura, generalmente se abre el archivo, se lee línea por línea y se cierra el archivo, como hemos hecho en nuestro ejemplo.

## Ver También 

1. Documentación oficial de Java para la clase BufferedReader: https://docs.oracle.com/javase/7/docs/api/java/io/BufferedReader.html
2. Diferencias entre BufferedReader y Scanner: https://www.geeksforgeeks.org/difference-between-scanner-and-bufferedreader-class-in-java/
3. java.io.FileReader: https://docs.oracle.com/javase/7/docs/api/java/io/FileReader.html
4. Procesamiento eficiente de archivos en Java: https://dzone.com/articles/efficient-file-io-in-java.