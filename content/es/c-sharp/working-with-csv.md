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

## ¿Qué y por qué?
Trabajar con CSV significa manipular datos en formato CSV (Valores Separados por Comas, por sus siglas en inglés). Los programadores utilizan esta técnica para leer y escribir datos en tablas simples, una forma eficiente de organizar datos.

## ¿Cómo hacerlo?
Aquí encontrarás algunas muestras sencillas para manipular archivos CSV en C#:

### Cargar datos desde un archivo CSV:
```
using System;
using System.IO;

class Program
{
  static void Main()
  {
    // Lee cada línea del archivo y la imprime en la consola
    using (StreamReader sr = new StreamReader("datos.csv"))
    {
      string linea;
      while((linea = sr.ReadLine()) != null)
      {
         Console.WriteLine(linea);
      }
    }
  }
}
```

**Salida:**
```
Nombre, Edad, Ciudad
Juan, 25, Madrid
María, 30, Barcelona
```

### Escribir datos en un archivo CSV:
```
using System;
using System.IO;

class Program
{
  static void Main()
  {
    // Arrays con los datos que queremos escribir
    string[] nombres = {"Juan", "María"};
    int[] edades = {25, 30};
    string[] ciudades = {"Madrid", "Barcelona"};

    // Escribe los datos en un archivo CSV
    using (StreamWriter sw = new StreamWriter("nuevos_datos.csv"))
    {
      for (int i = 0; i < nombres.Length; i++)
      {
        sw.WriteLine($"{nombres[i]},{edades[i]},{ciudades[i]}");
      }
    }
  }
}
```

**Salida (en el archivo nuevos_datos.csv):**
```
Juan, 25, Madrid
María, 30, Barcelona
```

## Profundizando
El formato CSV se originó en los años 70, cuando las tabulaciones y los espacios se utilizaban para separar datos en lugar de comas. Aunque se consideraba obsoleto por un tiempo, su simplicidad y versatilidad lo han mantenido como una forma popular para almacenar y compartir datos.

Alternativas al formato CSV incluyen bases de datos relacionales y formatos como JSON, XML y YAML. Sin embargo, CSV sigue siendo una opción eficiente y de fácil acceso para manipular datos en tablas.

Si deseas trabajar con archivos CSV en C#, también puedes considerar las librerías externas como CsvHelper y LumenWorksCsvReader, que ofrecen funcionalidades adicionales como lectura y escritura en archivos grandes, mapeo de datos a tipos específicos y manejo de errores.

## Ver también
- [Documentación oficial de C# sobre lectura y escritura de archivos CSV](https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/file-system/how-to-read-and-write-to-a-newly-created-data-file)
- [CsvHelper library](https://joshclose.github.io/CsvHelper/)
- [LumenWorksCsvReader library](https://github.com/phatcher/CsvReader)