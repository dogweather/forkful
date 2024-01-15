---
title:                "Trabajando con csv"
html_title:           "Java: Trabajando con csv"
simple_title:         "Trabajando con csv"
programming_language: "Java"
category:             "Java"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/java/working-with-csv.md"
---

{{< edit_this_page >}}

¿Por qué trabajar con CSV en Java?

Trabajar con archivos CSV en Java puede ser extremadamente útil en muchas situaciones, especialmente cuando se trata de manejar grandes cantidades de datos estructurados. Esto puede ser particularmente útil para aquellos que trabajan con análisis de datos o bases de datos.

## Cómo hacerlo

Para trabajar con archivos CSV en Java, primero necesitarás importar la clase **CSVReader** de la librería **opencsv**. Una vez que hayas hecho esto, puedes crear una instancia de **CSVReader** utilizando un objeto de tipo **FileReader** y pasando como parámetro el archivo CSV que deseas leer.

Aquí hay un ejemplo de cómo leer un archivo CSV y mostrar su contenido en la consola:

```Java
import java.io.FileReader;
import com.opencsv.CSVReader;

public class CSVExample {
  public static void main(String[] args) {
    try {
      // Crear objeto CSVReader y pasar el archivo CSV que deseas leer
      CSVReader reader = new CSVReader(new FileReader("ejemplo.csv"));
      String[] linea;

      // Leer cada línea del archivo y mostrarla en la consola
      while ((linea = reader.readNext()) != null) {
        for (String dato : linea) {
          System.out.print(dato + " ");
        }
        System.out.println();
      }
      reader.close();
    } catch (Exception e) {
      e.printStackTrace();
    }
  }
}
```

El resultado de este programa sería:

```bash
1, Juan, Pérez, juan@email.com
2, María, García, maria@email.com
3, Pedro, López, pedro@email.com
```

Además de leer archivos CSV, también es posible escribir en ellos utilizando la clase **CSVWriter**. Aquí hay un ejemplo de cómo crear un nuevo archivo CSV y escribir en él:

```Java
import java.io.FileWriter;
import java.util.ArrayList;
import com.opencsv.CSVWriter;

public class CSVWriterExample {
  public static void main(String[] args) {
    try {
      // Crear objeto CSVWriter y pasar el nombre del archivo a crear
      CSVWriter writer = new CSVWriter(new FileWriter("nuevo.csv"));

      // Crear una lista de datos para agregar al archivo CSV
      ArrayList<String[]> datos = new ArrayList<String[]>();
      datos.add(new String[] {"ID", "Nombre", "Apellido", "Email"});
      datos.add(new String[] {"1", "Juan", "Pérez", "juan@email.com"});
      datos.add(new String[] {"2", "María", "García", "maria@email.com"});
      datos.add(new String[] {"3", "Pedro", "López", "pedro@email.com"});

      // Escribir los datos en el archivo CSV
      writer.writeAll(datos);

      writer.close();
    } catch (Exception e) {
      e.printStackTrace();
    }
  }
}
```

El resultado sería un nuevo archivo CSV llamado "nuevo.csv" con la siguiente información:

```bash
ID, Nombre, Apellido, Email
1, Juan, Pérez, juan@email.com
2, María, García, maria@email.com
3, Pedro, López, pedro@email.com
```

## Profundizando en CSV

Ahora que tienes una idea básica de cómo leer y escribir archivos CSV en Java, es importante comprender que existen diferentes métodos y opciones para trabajar con ellos. Puedes, por ejemplo, especificar un delimitador personalizado para el archivo CSV o incluso utilizar la clase **CSVParser** para analizar los datos de diferentes maneras.

También es importante tener en cuenta que, aunque CSV es un formato de archivo muy utilizado, no es la mejor opción para todos los tipos de datos. Por ejemplo, si tus datos contienen valores de texto que pueden incluir comas o saltos de línea, podrías tener problemas al leer o escribir el archivo CSV.

Por esta razón, es importante investigar y comprender todas las opciones disponibles para trabajar con archivos CSV en Java y elegir la mejor opción según tus necesidades específicas.

## Ver también

- [Documentación oficial de OpenCSV](http://opencsv.sourceforge.net/)
- [Tutorial de Java CSV](https://www.baeldung.com/java-csv)
- [Especificar delimitador en Java CSV](https://stackoverflow.com/questions/19881224/set-delimiter-in-opencsv?rq=1)