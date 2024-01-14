---
title:                "Java: Trabajando con csv"
simple_title:         "Trabajando con csv"
programming_language: "Java"
category:             "Java"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/java/working-with-csv.md"
---

{{< edit_this_page >}}

# ¿Por qué trabajar con CSV? 

CSV (Valores Separados por Comas) es uno de los formatos de archivos más comunes y utilizados en el mundo de la programación. Este formato permite almacenar y organizar datos de forma sencilla y estructurada, lo que lo hace muy útil en diferentes aplicaciones. En Java, trabajar con archivos CSV puede resultar beneficioso para realizar tareas de análisis de datos, exportar e importar información, entre otros. En esta entrada, te mostraremos cómo trabajar con CSV en Java y los beneficios que puede aportar a tus proyectos.

## ¿Cómo hacerlo?

Para empezar, debemos tener en cuenta que en Java, para trabajar con archivos CSV, necesitaremos importar la clase `CSVReader` de la librería `opencsv`. A continuación, te mostramos un ejemplo de cómo leer un archivo CSV y mostrar su contenido por consola:

```Java
import java.io.FileReader;
import java.io.IOException;
import java.util.List;
import com.opencsv.CSVReader;

public class LeerCSV {

    public static void main(String[] args) throws IOException {

        // Se define la ruta del archivo CSV
        String ruta = "datos.csv";

        // Se crea un objeto CSVReader para leer el archivo
        CSVReader reader = new CSVReader(new FileReader(ruta));

        // Se convierte el contenido del archivo a una lista de arrays
        List<String[]> contenido = reader.readAll();

        // Se recorre la lista y se imprime cada array en una línea diferente
        for (String[] fila : contenido) {
            System.out.println(Arrays.toString(fila));
        }

        // Se cierra el objeto CSVReader
        reader.close();

    }

}
```

Este código nos permite leer el archivo CSV y mostrar su contenido por consola, lo que resulta útil para poder manipular estos datos en el futuro. Además, utilizando la librería `opencsv` podemos realizar otras operaciones como escribir en archivos CSV o realizar búsquedas específicas de datos.

## ¡Sumérgete en los detalles!

Ahora que ya sabemos cómo leer un archivo CSV en Java, es importante conocer algunas características importantes de este formato de archivo. En primer lugar, los archivos CSV suelen tener como delimitador la coma (`,`), sin embargo, puede variar según la configuración regional del sistema en el que se esté trabajando. Además, es común que los datos estén separados por columnas y cada fila represente un registro completo de información.

En cuanto a la manipulación de datos en archivos CSV, es importante tener en cuenta que cualquier valor que contenga una coma o una nueva línea debe estar entre comillas para que sea considerado como un solo campo. También es importante estar atentos a posibles caracteres especiales que puedan afectar la lectura del archivo.

¡No olvides explorar las diferentes opciones y métodos que ofrece la librería `opencsv` para manipular archivos CSV en Java!

## Ver también

- [Documentación de la librería opencsv en español](https://opencsv.es/)
- [Ejemplos de código para trabajar con archivos CSV en Java](https://github.com/Baeldung/opencsv-tutorial)