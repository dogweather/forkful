---
title:                "Trabajando con archivos CSV"
date:                  2024-01-19
html_title:           "Bash: Trabajando con archivos CSV"
simple_title:         "Trabajando con archivos CSV"

category:             "Java"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/java/working-with-csv.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?

Trabajar con archivos CSV implica leer y escribir datos en formato de texto con valores separados por comas. Los programadores lo hacen porque es un estándar simple y ampliamente adoptado para intercambiar datos, fácil de entender y usar tanto para humanos como para máquinas.

## Cómo hacerlo:

Leer un CSV en Java:
```java
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

public class CsvReader {
    public static void main(String[] args) {
        String path = "datos.csv";
        String line = "";
        
        try (BufferedReader br = new BufferedReader(new FileReader(path))) {
            while ((line = br.readLine()) != null) {
                String[] values = line.split(",");
                // Haz algo con los valores aquí
                System.out.println("Columna 1: " + values[0]);
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
```
Salida:
```
Columna 1: Valor1
Columna 1: Valor2
...
```

Escribir un CSV en Java:
```java
import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.IOException;

public class CsvWriter {
    public static void main(String[] args) {
        String[] data = {"Primera línea", "Segunda línea", "Tercera línea"};

        try (BufferedWriter bw = new BufferedWriter(new FileWriter("salida.csv"))) {
            for(String line : data) {
                bw.write(line);
                bw.newLine();
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
```

## Profundización

El formato CSV se originó en la década de 1970, pero todavía se usa debido a su simplicidad. Aunque hay alternativas como JSON y XML, CSV sigue siendo relevante por su facilidad de lectura y escritura. En la implementación hay que tener en cuenta posibles complicaciones como campos que contienen comas, saltos de línea o la necesidad de manejar el juego de caracteres adecuado (encoding).

## Ver También

- Especificación RFC 4180 para CSV: https://tools.ietf.org/html/rfc4180
- Librería Apache Commons CSV para manejo avanzado de CSV en Java: https://commons.apache.org/proper/commons-csv/
- Tutorial para manejo de archivos CSV en Java con OpenCSV: http://opencsv.sourceforge.net/
