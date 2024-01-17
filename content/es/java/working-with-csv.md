---
title:                "Trabajando con archivos csv"
html_title:           "Java: Trabajando con archivos csv"
simple_title:         "Trabajando con archivos csv"
programming_language: "Java"
category:             "Java"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/java/working-with-csv.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

CSV, o Comma-Separated Values, es un formato de archivo de texto que se utiliza para almacenar datos en una tabla, donde cada valor está separado por una coma. Los programadores suelen trabajar con archivos CSV porque es un formato simple y fácil de leer y escribir, lo que lo hace ideal para almacenar, editar y compartir grandes cantidades de datos.

## Cómo hacerlo:

El siguiente ejemplo muestra cómo podemos leer un archivo CSV usando la clase BufferedReader y el método split de la clase String en Java.

```java
// Importa las clases necesarias
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

// Define el nombre y la ubicación del archivo CSV
String csvFile = "datos.csv";

// Crea un objeto BufferedReader para leer el archivo
BufferedReader br = null;

// Define una cadena para almacenar cada línea del archivo
String line = "";

// Define el separador para los valores del CSV
// En este caso, es una coma (,)
String separator = ",";

// Intenta leer el archivo
try {

    // Inicializa el objeto BufferedReader con el archivo CSV
    br = new BufferedReader(new FileReader(csvFile));

    // Lee cada línea del archivo
    while ((line = br.readLine()) != null) {

        // Separa los valores por el separador
        String[] values = line.split(separator);

        // Imprime cada valor
        for (String value : values) {
            System.out.println(value);
        }
    }

// Captura y maneja los errores
} catch (IOException e) {
    e.printStackTrace();
} finally {
    // Cierra el objeto BufferedReader
    if (br != null) {
        try {
            br.close();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
```

El ejemplo anterior imprimirá cada valor en una nueva línea en la consola.

## Profundizando:

El formato de archivo CSV se originó en 1972 y se utilizó por primera vez en Visicalc, una de las primeras hojas de cálculo electrónicas. Aunque CSV es un formato simple y ampliamente utilizado, puede haber algunos problemas con la forma en que se manejan los datos en diferentes programas. Algunos programas pueden interpretar los valores de una manera diferente, lo que puede causar problemas con la consistencia de datos.

En cuanto a alternativas, existen otros formatos de archivo como Excel, JSON o XML que pueden ser mejores opciones según el caso de uso. Sin embargo, CSV sigue siendo popular debido a su simplicidad y compatibilidad con la mayoría de los programas y lenguajes de programación.

Para implementar la lectura y escritura de archivos CSV de manera más eficiente, es recomendable utilizar librerías de terceros como OpenCSV, Apache Commons CSV o Super CSV, que ofrecen una variedad de funciones y métodos para trabajar con este formato de archivo.

## Ver también:

- [OpenCSV](http://opencsv.sourceforge.net)
- [Apache Commons CSV](http://commons.apache.org/proper/commons-csv/)
- [Super CSV](https://super-csv.github.io/super-csv/index.html)