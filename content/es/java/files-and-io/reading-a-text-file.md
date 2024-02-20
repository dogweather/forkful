---
date: 2024-01-20 17:54:42.987996-07:00
description: "Leer un archivo de texto en Java significa extraer la informaci\xF3\
  n contenida en \xE9l. Los programadores lo hacen para trabajar con datos, configuraciones\
  \ o\u2026"
lastmod: 2024-02-19 22:05:17.477036
model: gpt-4-1106-preview
summary: "Leer un archivo de texto en Java significa extraer la informaci\xF3n contenida\
  \ en \xE9l. Los programadores lo hacen para trabajar con datos, configuraciones\
  \ o\u2026"
title: Lectura de un archivo de texto
---

{{< edit_this_page >}}

## ¿Qué & Por Qué?
Leer un archivo de texto en Java significa extraer la información contenida en él. Los programadores lo hacen para trabajar con datos, configuraciones o para procesar información externa.

## Cómo hacerlo:
Empezamos con un ejemplo básico usando `java.nio.file.Files` para leer todas las líneas de un archivo:

```java
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.List;

public class ReadTextFile {
    public static void main(String[] args) {
        String filePath = "example.txt"; // Ruta al archivo de texto
        try {
            List<String> lines = Files.readAllLines(Paths.get(filePath));
            for (String line : lines) {
                System.out.println(line);
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
```

Salida de ejemplo:
```
Primera línea
Segunda línea
Tercera línea
```

## Profundizando:
Históricamente, Java proporcionaba clases como `FileReader` y `BufferedReader` para leer archivos:

```java
import java.io.BufferedReader;
import java.io.FileReader;

public class ReadTextFileLegacy {
    public static void main(String[] args) {
        String filePath = "example.txt";
        try (BufferedReader reader = new BufferedReader(new FileReader(filePath))) {
            String line;
            while ((line = reader.readLine()) != null) {
                System.out.println(line);
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
```

Con Java 8 se introdujo `java.nio.file`, ofreciendo una nueva manera de operar con archivos y directorios que es más moderna y eficiente en comparación a los métodos más antiguos. Además, para archivos grandes, es común usar `Files.lines()` que devuelve un `Stream` permitiendo operaciones más complejas de procesamiento en flujo.

Alternativas incluyen bibliotecas de terceros como Apache Commons IO o Google's Guava, que simplifican aún más el manejo de archivos.

Detalles de implementación: al leer archivos, siempre maneja excepciones y codificaciones de caracteres. Usa `try-with-resources` para asegurar que el recurso de archivo se cierre apropiadamente después de su uso.

## Ver También:
- Documentation for `java.nio.file.Files`: [https://docs.oracle.com/javase/8/docs/api/java/nio/file/Files.html](https://docs.oracle.com/javase/8/docs/api/java/nio/file/Files.html)
- Java I/O Tutorial: [https://docs.oracle.com/javase/tutorial/essential/io/](https://docs.oracle.com/javase/tutorial/essential/io/)
- Apache Commons IO: [https://commons.apache.org/proper/commons-io/](https://commons.apache.org/proper/commons-io/)
- Guava's Files Explained: [https://github.com/google/guava/wiki/IOExplained#files](https://github.com/google/guava/wiki/IOExplained#files)
