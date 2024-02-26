---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:19:00.274319-07:00
description: "Trabajar con archivos CSV (Valores Separados por Comas) se trata de\
  \ procesar y manipular datos almacenados en un formato de texto simple, donde cada\
  \ l\xEDnea\u2026"
lastmod: '2024-02-25T18:49:55.867077-07:00'
model: gpt-4-0125-preview
summary: "Trabajar con archivos CSV (Valores Separados por Comas) se trata de procesar\
  \ y manipular datos almacenados en un formato de texto simple, donde cada l\xED\
  nea\u2026"
title: Trabajando con CSV
---

{{< edit_this_page >}}

## ¿Qué y por qué?

Trabajar con archivos CSV (Valores Separados por Comas) se trata de procesar y manipular datos almacenados en un formato de texto simple, donde cada línea del texto representa una fila en una tabla, y las comas separan las columnas individuales. Los programadores utilizan esto para importar, exportar y gestionar datos a través de diferentes sistemas debido a la amplia aceptación de CSV como un formato de intercambio de datos ligero, legible por humanos.

## Cómo:

### Leer un archivo CSV usando la Biblioteca Estándar de C++:

```cpp
#include <fstream>
#include <iostream>
#include <sstream>
#include <vector>

int main() {
    std::ifstream file("data.csv");
    std::string line;
    
    while (std::getline(file, line)) {
        std::stringstream lineStream(line);
        std::string cell;
        std::vector<std::string> parsedRow;
        
        while (std::getline(lineStream, cell, ',')) {
            parsedRow.push_back(cell);
        }
        
        // Procesar parsedRow aquí
        for (const auto& val : parsedRow) {
            std::cout << val << "\t";
        }
        std::cout << std::endl;
    }
    
    return 0;
}
```

### Escribir en un archivo CSV:

```cpp
#include <fstream>
#include <vector>

int main() {
    std::ofstream file("output.csv");
    std::vector<std::vector<std::string>> data = {
        {"Nombre", "Edad", "Ciudad"},
        {"John Doe", "29", "Nueva York"},
        {"Jane Smith", "34", "Los Ángeles"}
    };
    
    for (const auto& fila : data) {
        for (size_t i = 0; i < fila.size(); i++) {
            file << fila[i];
            if (i < fila.size() - 1) file << ",";
        }
        file << "\n";
    }
    
    return 0;
}
```

### Usando una biblioteca de terceros: `csv2`:

Mientras que la Biblioteca Estándar de C++ proporciona las herramientas básicas para trabajar con archivos y cadenas, aprovechar bibliotecas de terceros puede simplificar el procesamiento de CSV. Una de estas bibliotecas es `csv2`, conocida por su facilidad de uso y eficiencia.

- Instalación: Generalmente instalada a través de gestores de paquetes como Conan o directamente desde su repositorio en GitHub.

Ejemplo usando `csv2` para leer un archivo CSV:

```cpp
#include <csv2/reader.hpp>
#include <iostream>

int main() {
    csv2::Reader<csv2::delimiter<','>, csv2::quote_character<'"'>, csv2::first_row_is_header<true>> csv;
    if (csv.mmap("data.csv")) {
        const auto encabezado = csv.header();
        for (const auto fila : csv) {
            for (const auto cell : fila) {
                std::cout << cell.second << "\t"; // Imprime el valor de cada celda
            }
            std::cout << std::endl;
        }
    }
    return 0;
}
```

La salida de ejemplo para las operaciones de lectura podría verse así (asumiendo un archivo CSV simple de tres columnas):

```
John    29    Nueva York    
Jane    34    Los Ángeles
```

Estos ejemplos buscan cubrir operaciones fundamentales de CSV en C++. Para escenarios más complejos, como tratar con archivos grandes o transformaciones de datos complejas, podría justificarse la exploración adicional en bibliotecas especializadas o herramientas.
