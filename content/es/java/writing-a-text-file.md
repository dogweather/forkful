---
title:    "Java: Escribiendo un archivo de texto"
keywords: ["Java"]
---

{{< edit_this_page >}}

## ¿Por qué escribir un archivo de texto?

Escribir un archivo de texto es una tarea común en la programación Java. Puede ser útil para almacenar datos de forma persistente, para poder acceder a ellos en el futuro o para compartirlos con otros programas o personas. Además, los archivos de texto son fáciles de crear y leer, lo que los convierte en una opción popular para almacenar información.

## Cómo hacerlo

Para escribir un archivo de texto en Java, primero es necesario importar la clase File y FileWriter. Luego, se debe crear una instancia de la clase File con el nombre y la ubicación del archivo deseado. A continuación, se debe crear una instancia de FileWriter con la instancia de File como parámetro. Finalmente, utilizando el método write(), se puede escribir el contenido deseado en el archivo. A continuación se muestra un ejemplo de código:

```Java
import java.io.File;
import java.io.FileWriter;

public class WritingTextFile {
    public static void main(String[] args) {
        // Crear objeto File con el nombre y ubicación del archivo
        File file = new File("myFile.txt");
        
        // Crear objeto FileWriter con la instancia de File como parámetro
        FileWriter writer = new FileWriter(file);
        
        // Escribir contenido en el archivo
        writer.write("Este es un archivo de texto de ejemplo.");
        
        // Cerrar FileWriter
        writer.close();
    }
}
```

El resultado de este código será un archivo de texto llamado "myFile.txt" con el texto "Este es un archivo de texto de ejemplo." escrito en él.

## Profundizando

Además de escribir texto en un archivo, también es posible leer texto de un archivo existente en Java. Esto se puede lograr utilizando la clase FileReader. También es importante tener en cuenta que cuando se utiliza el método write(), es necesario agregar una nueva línea al final del contenido deseado para que el texto se escriba en líneas separadas.

Otra cosa a considerar es la codificación del archivo de texto. Por defecto, Java utilizará la codificación predeterminada del sistema, pero es posible establecer una codificación específica utilizando un objeto OutputStreamWriter.

## Ver también

- [Tutorial de Java sobre escritura y lectura de archivos de texto] (https://www.geeksforgeeks.org/different-ways-to-write-data-into-a-file-in-java/)
- [Documentación oficial de Java sobre la clase FileWriter] (https://docs.oracle.com/javase/7/docs/api/java/io/FileWriter.html)
- [Documentación oficial de Java sobre la clase FileReader] (https://docs.oracle.com/javase/7/docs/api/java/io/FileReader.html)