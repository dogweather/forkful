---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:20:15.701735-07:00
description: "C\xF3mo hacerlo: Java no tiene soporte incorporado para CSV en su biblioteca\
  \ est\xE1ndar, pero puedes leer f\xE1cilmente un archivo CSV utilizando clases de\u2026"
lastmod: '2024-03-13T22:44:58.959803-06:00'
model: gpt-4-0125-preview
summary: "Java no tiene soporte incorporado para CSV en su biblioteca est\xE1ndar,\
  \ pero puedes leer f\xE1cilmente un archivo CSV utilizando clases de `java.io`."
title: Trabajando con CSV
weight: 37
---

## Cómo hacerlo:


### Leyendo un archivo CSV utilizando la biblioteca estándar de Java
Java no tiene soporte incorporado para CSV en su biblioteca estándar, pero puedes leer fácilmente un archivo CSV utilizando clases de `java.io`.

```java
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

public class ReadCSVExample {
    public static void main(String[] args) {
        String line;
        String csvFile = "data.csv"; // Especificar la ruta al archivo CSV
        try (BufferedReader br = new BufferedReader(new FileReader(csvFile))) {
            while ((line = br.readLine()) != null) {
                String[] values = line.split(","); // Asumiendo que una coma es el delimitador
                // Procesar los datos
                for (String value : values) {
                    System.out.print(value + " ");
                }
                System.out.println();
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
```

### Escribiendo en un archivo CSV utilizando la biblioteca estándar de Java
Para escribir datos en un archivo CSV, puedes usar clases de `java.io` tales como `FileWriter` y `BufferedWriter`.

```java
import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.IOException;

public class WriteCSVExample {
    public static void main(String[] args) {
        String[] data = {"John", "Doe", "30", "New York"};
        String csvFile = "output.csv"; // Especificar la ruta al archivo CSV de salida

        try (BufferedWriter bw = new BufferedWriter(new FileWriter(csvFile))) {
            StringBuilder sb = new StringBuilder();
            for (String value : data) {
                sb.append(value).append(","); // Asumiendo que una coma es el delimitador
            }
            sb.deleteCharAt(sb.length() - 1); // Remover la última coma
            bw.write(sb.toString());
            bw.newLine();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
```

### Usando una biblioteca de terceros: Apache Commons CSV
Apache Commons CSV es una biblioteca popular para manejar archivos CSV en Java. Simplifica significativamente la lectura y escritura de archivos CSV.

Agrega la dependencia a tu proyecto:

Para Maven:

```xml
<dependency>
    <groupId>org.apache.commons</groupId>
    <artifactId>commons-csv</artifactId>
    <version>1.9.0</version> <!-- Verificar la última versión -->
</dependency>
```

#### Leyendo un archivo CSV:
```java
import org.apache.commons.csv.CSVFormat;
import org.apache.commons.csv.CSVParser;
import org.apache.commons.csv.CSVRecord;

import java.io.Reader;
import java.io.FileReader;
import java.io.IOException;

public class ApacheReadCSVExample {
    public static void main(String[] args) {
        String csvFile = "data.csv";
        try (Reader reader = new FileReader(csvFile);
             CSVParser csvParser = new CSVParser(reader, CSVFormat.DEFAULT)) {
            for (CSVRecord csvRecord : csvParser) {
                // Accediendo a los valores por los índices de columnas
                String columnOne = csvRecord.get(0);
                String columnTwo = csvRecord.get(1);
                System.out.println(columnOne + " " + columnTwo);
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
```

#### Escribiendo en un archivo CSV:
```java
import org.apache.commons.csv.CSVFormat;
import org.apache.commons.csv.CSVPrinter;

import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.IOException;

public class ApacheWriteCSVExample {
    public static void main(String[] args) {
        String[] headers = {"Nombre", "Apellido", "Edad", "Ciudad"};
        String[] data = {"John", "Doe", "30", "New York"};

        try (BufferedWriter writer = new BufferedWriter(new FileWriter("output.csv"));
             CSVPrinter csvPrinter = new CSVPrinter(writer, CSVFormat.DEFAULT.withHeader(headers))) {
            csvPrinter.printRecord((Object[]) data); // Es necesario el casting a Object[] aquí
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
```

Apache Commons CSV maneja complejidades como las comillas y comas dentro de los campos automáticamente, haciéndolo una elección robusta para la manipulación de archivos CSV en Java.
