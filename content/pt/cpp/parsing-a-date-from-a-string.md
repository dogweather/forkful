---
title:                "Extraindo uma data de uma string."
html_title:           "C++: Extraindo uma data de uma string."
simple_title:         "Extraindo uma data de uma string."
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/cpp/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## O que é e Por Que?

A análise de uma data a partir de uma string é o processo de extrair informações de uma string que representa uma data e transformá-la em um formato que possa ser facilmente manipulado pelo programa. Isso é útil para realizar cálculos ou comparações de datas em programas, como em um aplicativo de calendário ou para verificar se uma data é válida. 

## Como fazer:

Veja abaixo um exemplo de como realizar a análise de uma data a partir de uma string em C++:

```C++
#include <iostream>
#include <chrono> // biblioteca para lidar com datas e tempo
#include <ctime>
#include <string>

using namespace std;

int main() {
    // uma string representando uma data
    string data = "10/02/2020";

    // variáveis para armazenar os valores da data
    int dia, mes, ano;

    // utilizando a função sscanf para extrair os valores da string
    sscanf(data.c_str(), "%d/%d/%d", &dia, &mes, &ano);

    // imprimindo os valores extraídos
    cout << "Dia: " << dia << endl;
    cout << "Mês: " << mes << endl;
    cout << "Ano: " << ano << endl;

    return 0;
}
```

Saída:

```
Dia: 10
Mês: 02
Ano: 2020
```

## Aprofundamento:

A análise de uma data a partir de uma string é um processo importante na programação para lidar com informações de data e tempo. Antes do padrão de data ISO 8601 ser adotado, os formatos de data e hora variavam de acordo com o país e o sistema operacional, o que tornava necessário ter ferramentas para analisar esses diferentes formatos. Atualmente, existem bibliotecas e funções disponíveis para facilitar esse processo, como a função ```sscanf``` do C++ ou a biblioteca ```datetime``` do Python.

Além disso, a análise de uma data a partir de uma string pode ser considerada um exemplo da utilização de expressões regulares em programação. As expressões regulares são padrões de texto utilizados para buscar e manipular strings, sendo muito úteis para extrair informações de uma string no formato de data.

## Veja Também:

- [Documentação da Função ```sscanf``` do C++](https://www.cplusplus.com/reference/cstdio/sscanf/)
- [Documentação da Biblioteca ```datetime``` do Python](https://docs.python.org/3/library/datetime.html)
- [Tutorial sobre Expressões Regulares em C++](https://www.geeksforgeeks.org/regular-expressions-in-c-with-examples/)