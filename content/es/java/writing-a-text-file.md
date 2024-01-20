---
title:                "Escribiendo un archivo de texto"
html_title:           "Java: Escribiendo un archivo de texto"
simple_title:         "Escribiendo un archivo de texto"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/java/writing-a-text-file.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

Escribir un archivo de texto en Java es crear un archivo con contenido en forma de texto legible por humanos. Los programadores lo hacen para almacenar información, como configuraciones, registros o datos de entrada/salida, de forma persistente y accesible.

## ¿Cómo hacerlo?

Utilizando la clase `FileWriter` y su método `write()`, abrimos un archivo de texto y escribimos en él. Luego cerramos el archivo para asegurarnos de que todo se haya guardado correctamente. Aquí hay un ejemplo:

```Java
import java.io.FileWriter;
import java.io.IOException;

public class WriteFile {
   public static void main(String[] args) {
      try {
         FileWriter writer = new FileWriter("archivo.txt");
         writer.write("¡Hola amigos!");
         writer.close();
      } catch (IOException e) {
         e.printStackTrace();
      }
   }
}
```

El código anterior creará un archivo llamado `archivo.txt` que contiene el texto "¡Hola amigos!". Para ver el archivo, puedes usar cualquier editor de texto o el comando `cat` en la terminal.

## Profundizando

Escribir archivos de texto es una parte crucial de la programación en Java. Fue introducido en la versión 1.1 de Java y ha sido ampliamente utilizado desde entonces. Hay algunas alternativas, como el uso de la clase `PrintWriter` en lugar de `FileWriter`, pero la idea es la misma. Al escribir archivos de texto, también es importante tener en cuenta la codificación, ya que esto afectará cómo se verá el texto en diferentes dispositivos y sistemas operativos.

## Ver también

- [Clase FileWriter en la documentación de Java](https://docs.oracle.com/javase/7/docs/api/java/io/FileWriter.html)