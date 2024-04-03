---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:19:01.868413-07:00
description: 'Como fazer: #.'
lastmod: '2024-03-13T22:44:46.901587-06:00'
model: gpt-4-0125-preview
summary: '#.'
title: Trabalhando com CSV
weight: 37
---

## Como fazer:


### Lendo um arquivo CSV usando a Biblioteca Padrão do C++:
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
        
        // Processar parsedRow aqui
        for (const auto& val : parsedRow) {
            std::cout << val << "\t";
        }
        std::cout << std::endl;
    }
    
    return 0;
}
```

### Escrevendo em um arquivo CSV:
```cpp
#include <fstream>
#include <vector>

int main() {
    std::ofstream file("output.csv");
    std::vector<std::vector<std::string>> dados = {
        {"Nome", "Idade", "Cidade"},
        {"John Doe", "29", "Nova York"},
        {"Jane Smith", "34", "Los Angeles"}
    };
    
    for (const auto& linha : dados) {
        for (size_t i = 0; i < linha.size(); i++) {
            file << linha[i];
            if (i < linha.size() - 1) file << ",";
        }
        file << "\n";
    }
    
    return 0;
}
```

### Usando uma biblioteca de terceiros: `csv2`:
Enquanto a Biblioteca Padrão do C++ fornece as ferramentas básicas para trabalhar com arquivos e strings, utilizar bibliotecas de terceiros pode simplificar o processamento de CSV. Uma dessas bibliotecas é a `csv2`, conhecida por sua facilidade de uso e eficiência.

- Instalação: Geralmente instalada via gerenciadores de pacotes como Conan ou diretamente de seu repositório no GitHub.

Exemplo usando `csv2` para ler um arquivo CSV:

```cpp
#include <csv2/reader.hpp>
#include <iostream>

int main() {
    csv2::Reader<csv2::delimiter<','>, csv2::quote_character<'"'>, csv2::first_row_is_header<true>> csv;
    if (csv.mmap("data.csv")) {
        const auto cabeçalho = csv.header();
        for (const auto linha : csv) {
            for (const auto célula : linha) {
                std::cout << célula.second << "\t"; // Exibir o valor de cada célula
            }
            std::cout << std::endl;
        }
    }
    return 0;
}
```

A saída de amostra para operações de leitura pode parecer assim (assumindo um arquivo CSV simples de três colunas):

```
John    29    Nova York    
Jane    34    Los Angeles
```

Estes exemplos visam cobrir operações fundamentais com CSV em C++. Para cenários mais complexos, como lidar com arquivos grandes ou transformações de dados complexas, pode ser justificada uma exploração adicional em bibliotecas ou ferramentas especializadas.
