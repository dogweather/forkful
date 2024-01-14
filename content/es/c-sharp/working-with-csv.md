---
title:                "C#: Trabajando con csv"
simple_title:         "Trabajando con csv"
programming_language: "C#"
category:             "C#"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c-sharp/working-with-csv.md"
---

{{< edit_this_page >}}

## Por qué trabajar con CSV en C#

CSV (Comma-Separated Values) es un formato de archivo muy utilizado para almacenar datos en forma de tabla. Esto lo hace ideal para trabajar con grandes conjuntos de datos, ya que puede ser fácilmente leído y procesado por máquinas. En este artículo descubriremos por qué trabajar con CSV puede ser beneficioso para tus proyectos de programación en C#.

## Cómo trabajar con CSV en C#
Para trabajar con archivos CSV en C#, podemos utilizar la clase `TextFieldParser` del espacio de nombres `Microsoft.VisualBasic.FileIO`. Esta clase nos permite leer y analizar fácilmente los datos de CSV. Veamos un ejemplo de cómo podemos leer un archivo CSV y mostrar su contenido en la consola:

```C#
using Microsoft.VisualBasic.FileIO;

string ruta = "ejemplo.csv";

using (TextFieldParser parser = new TextFieldParser(ruta))
{
    parser.TextFieldType = FieldType.Delimited;
    parser.SetDelimiters(",");

    while (!parser.EndOfData)
    {
        string[] fila = parser.ReadFields();
        foreach (string dato in fila)
        {
            Console.Write($"{dato}\t");
        }
        Console.WriteLine();
    }
}
```

En el ejemplo anterior, estamos utilizando un `TextFieldParser` para leer un archivo CSV con el nombre "ejemplo.csv". Primero, configuramos el delimitador de campos como "," (coma), ya que usualmente los datos en un archivo CSV están separados por este carácter. Luego, utilizamos un bucle `while` para leer cada fila del archivo y mostrar sus datos en la consola.

El resultado de ejecutar este código con un archivo CSV de ejemplo podría ser el siguiente:

```
Nombre  Apellido    Edad    País
Juan    Pérez       25      México
María   García      30      España
```

Este es solo un ejemplo básico de cómo podemos trabajar con archivos CSV en C#. Sin embargo, existen muchas más opciones y funcionalidades que podemos usar dependiendo de nuestras necesidades.

## Inmersión profunda en el trabajo con CSV
Si deseas profundizar en el trabajo con archivos CSV en C#, puedes consultar la documentación oficial de Microsoft sobre la clase `TextFieldParser` y sus métodos. También puedes explorar otras opciones de librerías y paquetes disponibles en el mercado que facilitan el manejo de archivos CSV en C#.

En resumen, trabajar con CSV en C# puede ser muy beneficioso para tu proyecto, ya que te permite manejar grandes conjuntos de datos de manera sencilla y eficiente. Con la clase `TextFieldParser` y otros recursos disponibles, es posible leer, escribir y manipular archivos CSV en C# de manera profesional y efectiva.

## Ver también
- [Documentación de Microsoft sobre TextFieldParser](https://docs.microsoft.com/en-us/dotnet/api/microsoft.visualbasic.fileio.textfieldparser?view=net-5.0)
- [C# CSV Reader](https://github.com/schoentoon/CsvReader)
- [CSV Parser para .NET](https://www.nuget.org/packages/CsvHelper/)