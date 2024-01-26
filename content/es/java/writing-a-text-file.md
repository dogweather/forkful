---
title:                "Escritura de un archivo de texto"
html_title:           "Bash: Escritura de un archivo de texto"
simple_title:         "Escritura de un archivo de texto"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/java/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Qué y Por Qué?

Escribir un archivo de texto en Java significa almacenar datos en un formato legible por humanos y máquinas. Programadores lo hacen para guardar configuraciones, exportar datos y llevar un registro de lo ocurrido en la aplicación.

## Cómo hacerlo:

Escribimos archivos de texto con `Files.write()` que simplifica el proceso en pocas líneas.

```java
import java.nio.file.Files;
import java.nio.file.Paths;
import java.io.IOException;
import java.util.List;
import java.util.Arrays;

public class EscribirArchivo {
    public static void main(String[] args) {
        List<String> lineas = Arrays.asList("Primera línea", "Segunda línea");
        try {
            Files.write(Paths.get("miArchivo.txt"), lineas);
            System.out.println("Archivo escrito exitosamente.");
        } catch (IOException e) {
            System.out.println("Ocurrió un error al escribir el archivo.");
            e.printStackTrace();
        }
    }
}
```

Salida esperada: `Archivo escrito exitosamente.`

## Profundización

Históricamente, `FileWriter` y `BufferedWriter` fueron comunes para escribir archivos. Hoy, `Files.write()` es una alternativa moderna y más eficiente, parte de NIO (New Input/Output).

Alternativas como `FileOutputStream` pueden ser útiles para escribir datos binarios. Para operaciones complejas como procesar y escribir grandes volúmenes de datos, usar `BufferedWriter` es más eficiente en términos de memoria.

Detalles de implementación: `Files.write()` puede lanzar una `IOException`, por lo que necesita estar dentro de un bloque try-catch. Utiliza un `Charset` para codificar las cadenas en bytes. Por defecto, se usa UTF-8.

## Ver También

- [Documentación oficial de la clase Files](https://docs.oracle.com/en/java/javase/18/docs/api/java.base/java/nio/file/Files.html)
- [Tutorial de BufferedWriter](https://www.baeldung.com/java-write-to-file)
- [Guía de Java NIO](https://www.baeldung.com/java-nio-2-file-api)
