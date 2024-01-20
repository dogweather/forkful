---
title:                "Trabajando con csv"
html_title:           "C++: Trabajando con csv"
simple_title:         "Trabajando con csv"
programming_language: "C++"
category:             "C++"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/cpp/working-with-csv.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?
Trabajar con CSV (Comma-Separated Values) es una forma en que los programadores pueden almacenar y manipular grandes cantidades de datos en un formato de texto simple. Esto permite una fácil lectura y escritura de datos en aplicaciones y sistemas de bases de datos. Los programadores utilizan CSV para facilitar el procesamiento, la importación y la exportación de datos de manera eficiente.

## Cómo hacerlo:
Para trabajar con CSV en C++, necesitarás un archivo de encabezado como "fstream" para trabajar con archivos y "sstream" para procesar las cadenas. Primero, crea un archivo CSV con columnas y filas de datos separados por comas. Luego, utiliza un bucle para leer y almacenar los datos en tus variables. A continuación, puedes imprimir los datos en un formato deseado.

```C++
#include <fstream>
#include <sstream>

int main()
{
    // Leer archivo CSV
    ifstream archivo("datos.csv");
    string columna, fila;

    // Imprimir datos
    while(getline(archivo, columna))
    {
        stringstream sstr(columna);
        while(getline(sstr, fila, ','))
        {
            cout << fila << " ";
        }
        cout << endl;
    }
    archivo.close();

    return 0;
}
```

## Profundizando:
CSV se introdujo por primera vez en los años 70 como una forma de almacenar datos en hojas de cálculo. Hoy en día, es un formato de archivo ampliamente utilizado en aplicaciones de bases de datos y sistemas de gestión de contenido. Además de C++, hay otros lenguajes de programación que también pueden trabajar con archivos CSV, como Python y Java. Los desarrolladores pueden encontrar bibliotecas y paquetes de software que facilitan el trabajo con CSV.