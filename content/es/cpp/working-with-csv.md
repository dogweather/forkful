---
title:                "Trabajando con archivos CSV"
date:                  2024-01-19
html_title:           "Bash: Trabajando con archivos CSV"
simple_title:         "Trabajando con archivos CSV"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/cpp/working-with-csv.md"
---

{{< edit_this_page >}}

## ¿Qué & Por Qué?
Trabajar con CSV implica leer y escribir en archivos de texto que almacenan datos separados por comas. Los programadores lo hacen porque CSV es un formato sencillo y ampliamente compatible para intercambiar datos.

## Cómo Hacerlo:
```C++
#include <iostream>
#include <fstream>
#include <sstream>
#include <vector>

// Función para leer CSV
std::vector<std::vector<std::string>> leerCSV(const std::string& archivo) {
    std::vector<std::vector<std::string>> datos;
    std::ifstream file(archivo);
    std::string linea;
    
    while (std::getline(file, linea)) {
        std::stringstream ss(linea);
        std::vector<std::string> fila;
        std::string dato;
        
        while (std::getline(ss, dato, ',')) {
            fila.push_back(dato);
        }
        
        datos.push_back(fila);
    }
    
    return datos;
}

// Función para escribir CSV
void escribirCSV(const std::string& archivo, const std::vector<std::vector<std::string>>& datos) {
    std::ofstream file(archivo);
    
    for (const auto& fila : datos) {
        for (size_t i = 0; i < fila.size(); ++i) {
            file << fila[i];
            if (i < fila.size() - 1)
                file << ",";
        }
        file << "\n";
    }
}

int main() {
    // Asignar archivo CSV
    std::string archivo = "datos.csv";
    
    // Leer datos de CSV
    std::vector<std::vector<std::string>> leidos = leerCSV(archivo);
    
    // Escribir datos en CSV
    escribirCSV("nuevo.csv", leidos);
    
    return 0;
}
```
Output esperado: Un nuevo archivo "nuevo.csv" con los datos leídos del archivo original "datos.csv".

## Análisis Profundo:
CSV, acrónimo de Comma-Separated Values, data de los primeros días de la informática personal. Es simple pero no estandarizado, lo que puede llevar a inconsistencias. Alternativas como JSON o XML permiten una estructura más compleja y definida. El tratamiento de CSV en C++ no es inherentemente complejo pero requiere manejar correctamente la entrada/salida y el tratamiento de strings.

## Ver También:
- Documentación oficial de las bibliotecas IO de C++: http://www.cplusplus.com/reference/iostream/
- RFC 4180, definición común de CSV: https://tools.ietf.org/html/rfc4180
- Comparación de formatos de datos (JSON, XML, YAML, CSV): https://www.ubuntupit.com/json-vs-xml-vs-yaml-vs-csv-difference-comparison/
