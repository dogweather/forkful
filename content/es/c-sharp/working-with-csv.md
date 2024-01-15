---
title:                "Trabajando con csv"
html_title:           "C#: Trabajando con csv"
simple_title:         "Trabajando con csv"
programming_language: "C#"
category:             "C#"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c-sharp/working-with-csv.md"
---

{{< edit_this_page >}}

## ¿Por qué utilizar CSV en programación?

CSV (valores separados por comas) es un formato comúnmente utilizado para almacenar y compartir datos en una tabla simple. Es una forma sencilla y versátil de organizar información, por lo que es una herramienta útil para cualquier programador.

## Cómo utilizar CSV en C#

Crear y trabajar con archivos CSV en C# es muy sencillo. Aquí vamos a mostrarte algunos ejemplos de código para que puedas empezar a trabajar con este formato de datos.

```C#
//Importar la librería para trabajar con CSV
using System.IO;

//Escribir datos en un archivo CSV
using (StreamWriter sw = new StreamWriter("miArchivo.csv"))
{
    sw.WriteLine("Nombre, Edad, Ciudad"); //Primera fila con nombres de columnas
    sw.WriteLine("Pedro, 28, Madrid"); //Segunda fila con datos
    sw.WriteLine("María, 35, Barcelona");
}

//Leer y mostrar datos desde un archivo CSV
using (StreamReader sr = new StreamReader("miArchivo.csv"))
{
    string line;
    while((line = sr.ReadLine()) != null) //Recorre linea por linea hasta el final del archivo
    {
        string[] columns = line.Split(','); //Divide cada fila en las columnas según la coma
        Console.WriteLine("Nombre: " + columns[0] + ", Edad: " + columns[1] + ", Ciudad: " + columns[2]);
    }
    //Salida:
    //Nombre: Pedro, Edad: 28, Ciudad: Madrid
    //Nombre: María, Edad: 35, Ciudad: Barcelona
}
```

## Más información sobre CSV en C#

Además de leer y escribir datos en formato CSV, también podemos trabajar con librerías específicas para facilitar el manejo de esta estructura de datos, como por ejemplo CsvHelper. Esta librería nos permite mapear los datos de un archivo CSV a una clase C#, lo que facilita el acceso y manipulación de los mismos.

También es importante tener en cuenta que, aunque CSV es un formato muy utilizado y sencillo, no es adecuado para almacenar datos sensibles o relacionales complejos. En estos casos, sería más conveniente utilizar una base de datos.

## Ver también

- [Documentación oficial de Microsoft sobre CSV en C#](https://docs.microsoft.com/es-es/dotnet/visual-basic/programming-guide/fileio/how-to-read-from-comma-delimited-text-files)
- [Librería CsvHelper para C#](https://joshclose.github.io/CsvHelper/)
- [Formato CSV en Wikipedia](https://es.wikipedia.org/wiki/Valores_separados_por_comas)