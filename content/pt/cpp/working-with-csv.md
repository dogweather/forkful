---
title:                "Trabalhando com arquivos csv"
html_title:           "C++: Trabalhando com arquivos csv"
simple_title:         "Trabalhando com arquivos csv"
programming_language: "C++"
category:             "C++"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/cpp/working-with-csv.md"
---

{{< edit_this_page >}}

## O que e por que?

Trabalhar com CSV, ou "valores separados por vírgulas", é uma forma comum de organizar dados em um arquivo de texto. Os programadores geralmente trabalham com CSV para importar e exportar dados entre diferentes softwares ou para analisar grandes conjuntos de dados em linguagens de programação.

## Como fazer:

Para trabalhar com CSV em C++, você pode utilizar a biblioteca padrão do C++ e um pouco de manipulação de strings. Aqui está um exemplo de código que lê um arquivo CSV e imprime seus valores separados por vírgulas:

```C++
#include <iostream>
#include <fstream>
#include <sstream>
#include <string>
using namespace std;

int main() {
    ifstream file("dados.csv");
    
    string line;
    while(getline(file, line)) {
        istringstream iss(line);
        string valor;
        while (getline(iss, valor, ',')) {
            cout << valor << " ";
        }
        cout << "\n";
    }

    return 0;
}

```

Imagine que o arquivo CSV "dados.csv" tem o seguinte conteúdo:

```
João,Silva,28
Maria,Santos,35
```

A saída do programa seria:

```
João Silva 28
Maria Santos 35
```

## Deep Dive:

CSV foi criado como um formato de arquivo universal para troca de dados em um tempo onde a interoperabilidade entre softwares era um grande desafio. Além disso, CSV é um formato de arquivo simples e fácil de ler e editar mesmo em softwares de edição de texto simples.

Existem outras formas de trabalhar com dados estruturados, como XML e JSON, mas CSV se mantém popular devido à sua simplicidade e compatibilidade com diversas linguagens de programação.

O exemplo de código acima é bastante simples e pode ser otimizado para lidar com casos mais específicos, como dados com aspas ou com diferentes delimitadores.

## Veja também:

- [Biblioteca padrão do C++](https://en.cppreference.com/w/cpp/header)
- [Documentação sobre o formato CSV](https://tools.ietf.org/html/rfc4180)