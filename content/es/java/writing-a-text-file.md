---
title:                "Java: Escribiendo un archivo de texto"
simple_title:         "Escribiendo un archivo de texto"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/java/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Por qué escribir un archivo de texto

Escribir archivos de texto es una habilidad importante en la programación de Java. Puede ser útil para guardar información o resultados de un programa en un formato legible y fácil de manipular. También puede ser utilizado para generar informes o datos que se puedan compartir con otros.

## Cómo hacerlo

Escribir un archivo de texto en Java es muy sencillo. Todo lo que necesitas es seguir estos pasos:

1. Importa la clase `FileWriter` para manejar la escritura de archivos de texto.
2. Crea una instancia de `FileWriter` y proporciona la ubicación y el nombre del archivo que deseas crear.
3. Usa el método `write()` para escribir en el archivo, pasando como argumento el texto que deseas escribir.
4. Recuerda usar el método `close()` para cerrar el archivo después de escribir en él.

A continuación, se muestra un ejemplo de código básico que escribe un archivo de texto llamado "ejemplo.txt" con el texto "¡Hola, mundo!".

``` java
import java.io.FileWriter; // 1

public class EjemploArchivoTexto {
    public static void main(String[] args) {
        try {
            FileWriter archivo = new FileWriter("ejemplo.txt"); // 2
            archivo.write("¡Hola, mundo!"); // 3
            archivo.close(); // 4
        } catch (Exception e) {
            System.out.println("Ha ocurrido un error.");
            e.printStackTrace();
        }
    }
}
```

El resultado de este código será un archivo de texto llamado "ejemplo.txt" con el texto "¡Hola, mundo!" dentro.

## Profundizando

Hay varias cosas que debes tener en cuenta al escribir archivos de texto en Java. Por ejemplo, si deseas agregar más texto a un archivo existente, puedes usar el constructor sobrecargado de `FileWriter` y pasar `true` como segundo argumento. Esto indicará que deseas anexar al final del archivo en lugar de sobrescribirlo.

También es importante recordar cerrar el archivo después de escribir en él, ya que esto asegura que todos los cambios se escriban correctamente en el archivo.

## Ver también

- [Java FileWriter documentation](https://docs.oracle.com/javase/8/docs/api/java/io/FileWriter.html)
- [Difference between FileWriter and BufferedWriter](https://www.baeldung.com/java-filewriter-bufferedwriter)
- [Tutorial: Cómo escribir un archivo de texto en Java](https://www.solvetic.com/tutoriales/article/3904-como-escribir-un-archivo-de-texto-en-java/)