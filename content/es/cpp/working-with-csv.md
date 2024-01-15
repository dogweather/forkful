---
title:                "Trabajando con archivos csv"
html_title:           "C++: Trabajando con archivos csv"
simple_title:         "Trabajando con archivos csv"
programming_language: "C++"
category:             "C++"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/cpp/working-with-csv.md"
---

{{< edit_this_page >}}

## ¿Por qué?

El trabajo con archivos CSV es una habilidad esencial para cualquier programador de C++. CSV (Comma Separated Values) es un formato de archivo comúnmente utilizado para almacenar y manejar grandes cantidades de datos tabulares. Aprender a trabajar con archivos CSV es una manera eficiente de manejar y analizar datos para diversas aplicaciones.

## Cómo hacerlo

Aquí hay una demostración sencilla de cómo trabajar con archivos CSV en C++:

```C++
#include <iostream>
#include <fstream>
#include <sstream>
#include <vector>
#include <string>

using namespace std;

int main() {
  // Abrir el archivo CSV
  ifstream archivo("datos.csv");
  
  // Vector para almacenar los datos del archivo
  vector<vector<string>> datos;

  // Leer los datos del archivo línea por línea
  while (archivo.good()) {
    string linea;
    getline(archivo, linea);

    // Utilizar un stringstream para dividir la línea en columnas usando la coma como delimitador
    stringstream ss(linea);
    string columna;
    vector<string> fila;

    while (getline(ss, columna, ',')) {
      // Agregar cada columna al vector de filas
      fila.push_back(columna);
    }

    // Agregar la fila al vector de datos
    datos.push_back(fila);
  }

  // Imprimir los datos leídos del archivo
  for (int i = 0; i < datos.size(); i++) {
    for (int j = 0; j < datos[i].size(); j++) {
      cout << datos[i][j] << " ";
    }
    cout << endl;
  }

  return 0;
}
```

Este código abrirá un archivo CSV llamado "datos.csv" y guardará los datos en un vector de filas y columnas. Luego, los datos se imprimirán en la consola. 

### Ejemplo de datos

Supongamos que el archivo "datos.csv" contiene los siguientes datos:

```
Nombre, Edad, Ciudad
Ana, 25, Madrid
Juan, 30, Barcelona
María, 27, Valencia
```

El programa imprimirá lo siguiente:

```
Nombre Edad Ciudad
Ana 25 Madrid
Juan 30 Barcelona
María 27 Valencia
```

## Profundizando en el tema

Trabajar con archivos CSV puede ser mucho más complejo dependiendo de la cantidad de datos y su estructura. Por ejemplo, si hay datos faltantes en el archivo o si se requiere realizar cálculos o filtrar los datos. En estos casos, puede ser útil utilizar librerías específicas de C++ como "Easy CSV Parser" o "Libcsv" para facilitar el manejo de archivos CSV. También es importante aprender sobre los diferentes delimitadores y formatos de texto que pueden presentarse en los archivos CSV.

## Ver también

- [Easy CSV Parser](https://github.com/ben-strasser/fast-cpp-csv-parser)
- [Libcsv](hhttps://github.com/ben-strasser/libcsv)