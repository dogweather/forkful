---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:28:10.514260-07:00
description: "Escribir un archivo de texto en Java se trata de utilizar las capacidades\
  \ del lenguaje para crear y escribir contenido en archivos en el sistema de\u2026"
lastmod: '2024-03-13T22:44:58.955630-06:00'
model: gpt-4-0125-preview
summary: Escribir un archivo de texto en Java se trata de utilizar las capacidades
  del lenguaje para crear y escribir contenido en archivos en el sistema de archivos.
title: Escribiendo un archivo de texto
weight: 24
---

## Cómo:


### Usando `java.nio.file` (Biblioteca Estándar)
El paquete New I/O (NIO) de Java (`java.nio.file`) proporciona un enfoque más versátil para tratar con archivos. Aquí hay una manera simplista de escribir en un archivo usando `Files.write()`:

```java
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.List;

public class TextFileWriterNIO {
    public static void main(String[] args) {
        List<String> lines = Arrays.asList("Línea 1", "Línea 2", "Línea 3");
        try {
            Files.write(Paths.get("example.txt"), lines);
            System.out.println("¡Archivo escrito exitosamente!");
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
```

Salida:

```
¡Archivo escrito exitosamente!
```

### Usando `java.io` (Biblioteca Estándar)
Para un enfoque más tradicional, `java.io.FileWriter` es una buena elección para escribir archivos de texto simplemente:

```java
import java.io.FileWriter;
import java.io.IOException;

public class TextFileWriterIO {
    public static void main(String[] args) {
        try (FileWriter writer = new FileWriter("example.txt")) {
            writer.write("Hola, Mundo!\n");
            writer.append("Esta es otra línea.");
            System.out.println("¡Archivo escrito exitosamente!");
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
```

Salida:

```
¡Archivo escrito exitosamente!
```

### Usando Apache Commons IO
La biblioteca Apache Commons IO simplifica muchas operaciones, incluida la escritura de archivos. Así es como se escribe en un archivo usando `FileUtils.writeStringToFile()`:

Primero, agrega la dependencia a tu proyecto. Si usas Maven, incluye:

```xml
<dependency>
  <groupId>org.apache.commons</groupId>
  <artifactId>commons-io</artifactId>
  <version>2.11.0</version> <!-- Verifica la última versión -->
</dependency>
```

Luego, utiliza el siguiente código para escribir texto en un archivo:

```java
import org.apache.commons.io.FileUtils;
import java.io.File;
import java.io.IOException;

public class TextFileWriterCommonsIO {
    public static void main(String[] args) {
        try {
            FileUtils.writeStringToFile(new File("example.txt"), "Este es texto escrito usando Commons IO.", "UTF-8");
            System.out.println("¡Archivo escrito exitosamente!");
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
```

Salida:

```
¡Archivo escrito exitosamente!
```
