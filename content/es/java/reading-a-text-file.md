---
title:                "Java: Leyendo un archivo de texto"
programming_language: "Java"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/java/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Por qué leer un archivo de texto en Java

Leer archivos de texto es una tarea común en la programación de Java. Puede ser útil para leer datos de entrada, como configuraciones o datos almacenados en un archivo, o para analizar archivos de texto para extraer información específica. En esta entrada del blog, te explicaremos por qué es importante saber cómo leer archivos de texto en Java y cómo hacerlo.

## Cómo leer un archivo de texto en Java

Para leer un archivo de texto en Java, necesitarás seguir los siguientes pasos:

1. Abre el archivo utilizando la clase `FileReader` y el nombre del archivo como argumento.
```Java
FileReader fileReader = new FileReader("archivo.txt");
```
2. Utiliza la clase `BufferedReader` para leer el contenido del archivo línea por línea.
```Java
BufferedReader bufferedReader = new BufferedReader(fileReader);
```
3. Utiliza un ciclo `while` para leer cada línea del archivo y almacenarla en una variable.
```Java
while (bufferedReader.readLine() != null) {
  String linea = bufferedReader.readLine();
}
```
4. Cierra el objeto `BufferedReader` y el `FileReader` utilizando el método `close()` para liberar los recursos.
```Java
bufferedReader.close();
fileReader.close();
```

Con este código, podrás leer y almacenar el contenido del archivo de texto en una variable en tu programa de Java. A continuación, te mostraremos un ejemplo completo de cómo leer un archivo de texto y mostrar su contenido:

```Java
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

public class EjemploLeerArchivo {

    public static void main(String[] args) {
    
        // Definir el nombre del archivo
        String nombreArchivo = "archivo.txt";
        
        try {
        
            // Abrir el archivo
            FileReader fileReader = new FileReader(nombreArchivo);
            BufferedReader bufferedReader = new BufferedReader(fileReader);
            
            // Leer y mostrar el contenido del archivo
            String linea;
            while ((linea = bufferedReader.readLine()) != null) {
                System.out.println(linea);
            }
            
            // Cerrar el buffer y el FileReader
            bufferedReader.close();
            fileReader.close();
            
        } catch (IOException e) {
        
            System.out.println("Error al leer el archivo: " + e.getMessage());
        
        }
    }

}
```

### Profundizando en la lectura de archivos de texto

Si quieres profundizar un poco más en la lectura de archivos de texto en Java, hay algunas cosas que puedes tener en cuenta:

- Puedes utilizar la clase `Scanner` en lugar de la clase `BufferedReader` si quieres leer el archivo de una manera más estructurada.
- Puedes especificar un conjunto de caracteres para la codificación del archivo al abrirlo con `FileReader` para asegurarte de que estás leyendo correctamente los caracteres.
- Si estás leyendo archivos de gran tamaño, es posible que quieras considerar utilizar el método `read()` de la clase `FileReader` en lugar del método `readLine()` de `BufferedReader` para un mejor rendimiento.

Esperamos que este artículo te haya sido útil y te ayude a comprender mejor cómo leer archivos de texto en Java. Recuerda siempre cerrar el `BufferedReader` y el `FileReader` después de leer el archivo para evitar problemas de recursos.

## Ver También

- [Documentación de Oracle: Lectura, escritura y creación de archivos](https://docs.oracle.com/javase/tutorial/essential/io/file.html)
- [Tutorialspoint: Lectura y escritura de archivos en Java](https://www.tutorialspoint.com/java/io/java_io_read_write_operations.htm)
- [Mkyong: Tutorial sobre cómo leer y escribir archivos en Java](https://mkyong.com/java/how-to-read-file-in-java-fileinputstream/)

¡Feliz programación!