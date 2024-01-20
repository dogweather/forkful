---
title:                "Trabalhando com CSV"
html_title:           "Bash: Trabalhando com CSV"
simple_title:         "Trabalhando com CSV"
programming_language: "C++"
category:             "C++"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/cpp/working-with-csv.md"
---

{{< edit_this_page >}}

## O Que é & Por Que?

Trabalhar com CSV (Valores Separados por Vírgula) é lidar com um formato de arquivo simples para armazenar dados tabulares. Programadores usam CSV para importar e exportar dados de forma fácil e interoperável entre programas e sistemas.

## Como Fazer:

```cpp
#include <iostream>
#include <fstream>
#include <vector>
#include <sstream>

// Função para ler um arquivo CSV e imprimir o conteúdo
void lerCSV(const std::string& nomeArquivo) {
    std::ifstream arquivo(nomeArquivo);
    std::string linha;

    while (getline(arquivo, linha)) {
        std::istringstream iss(linha);
        std::string valor;
        
        // Separa os valores por vírgula e imprime
        while (getline(iss, valor, ',')) {
            std::cout << valor << " | ";
        }
        std::cout << '\n';
    }
}

int main() {
    // Substitua 'dados.csv' pelo seu arquivo CSV
    lerCSV("dados.csv");
    return 0;
}
```

O código acima lê o arquivo `'dados.csv'` e imprime o conteúdo em colunas separadas por `'|'`.

## Mergulho Profundo:

Historicamente, CSV é um dos formatos mais antigos usados para troca de dados tabulares, fácil de ser entendido e gerado por qualquer planilha ou banco de dados. Alternativas incluem JSON e XML, que são mais ricos em recursos, mas também mais complexos. Na implementação, atenção deve ser dada ao tratamento de exceções, como a presença de quebras de linha e vírgulas dentro dos campos de dados, algo que nosso exemplo rudimentar não cobre.

## Veja Também:

- Documentação C++ para leitura de arquivos: https://en.cppreference.com/w/cpp/io/basic_ifstream
- RFC 4180, definindo o formato CSV padrão: https://tools.ietf.org/html/rfc4180
- Biblioteca Boost para ler e escrever arquivos CSV: https://www.boost.org/doc/libs/release/libs/tokenizer/
- Documentação sobre o fluxo de entrada/saída em C++ (iostreams): https://en.cppreference.com/w/cpp/io