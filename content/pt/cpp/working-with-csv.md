---
title:                "C++: Trabalhando com csv"
simple_title:         "Trabalhando com csv"
programming_language: "C++"
category:             "C++"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/cpp/working-with-csv.md"
---

{{< edit_this_page >}}

## Por que trabalhar com CSV (Comma Separated Values)?

CSV é um formato de arquivo de dados amplamente utilizado, conhecido por sua simplicidade e fácil manipulação. Ele é amplamente utilizado para armazenar e transferir dados entre diferentes sistemas, tornando-se uma ferramenta valiosa para programadores em muitas linguagens de programação. Neste artigo, vamos explorar como trabalhar com CSV em C++.

## Como fazer

Para começar a trabalhar com CSV em C++, é necessário incluir a biblioteca de arquivos de fluxo de entrada e saída (iostream) no seu código. Em seguida, você precisará abrir o arquivo CSV usando a função ifstream, que irá permitir que você leia os dados do arquivo.

Em seguida, é necessário percorrer os dados linha por linha usando um loop. Isso pode ser feito usando a função getline, que irá ler uma linha inteira do arquivo e armazená-la em uma string.

Uma vez que as linhas são lidas, é possível dividir cada linha em suas respectivas colunas. Isso pode ser feito usando a função strtok, que divide a string com base em um caractere delimitador, neste caso, a vírgula (',').

Depois de ter acesso às colunas, é possível manipular os dados de acordo com as suas necessidades e fazer cálculos, comparações ou qualquer outra tarefa desejada.

Abaixo está um exemplo de código que ilustra os passos mencionados acima:

```C++
#include <iostream>
#include <fstream>
#include <cstring>

using namespace std;

int main() {
  ifstream csvFile("dados.csv");
  string linha;

  // loop que percorre cada linha do arquivo
  while (getline(csvFile, linha)) {
    char *coluna = strtok((char *)linha.c_str(), ",");

    // loop que percorre cada coluna da linha
    while (coluna != NULL) {
      // manipulação de dados da coluna aqui
      cout << coluna << " ";
      coluna = strtok(NULL, ",");
    }
    cout << endl;
  }
  return 0;
}
```

O código irá imprimir todas as linhas do arquivo CSV, separando cada coluna com um espaço.

## Mergulho profundo

Uma questão importante ao trabalhar com CSV é o tratamento de dados ausentes. Como os dados estão sendo lidos em formato textual, pode ser necessário tratar campos vazios ou nulos. Isso pode ser feito usando funções, como `getline` e `atof`, para verificar se o valor é válido e, caso contrário, atribuir um valor padrão ou ignorar a linha.

Além disso, é possível utilizar a biblioteca de expressões regulares (regex) para validar e extrair dados específicos de uma linha ou coluna, em vez de percorrer todas as colunas. Isso pode economizar tempo e tornar o código mais eficiente.

Outro ponto a ser considerado é a manipulação de grandes arquivos CSV. Se o arquivo for muito grande para ser armazenado na memória, pode ser necessário ler e processar as linhas em partes ou utilizar técnicas de páginação.

## Veja também

- Documentação C++ para leitura e escrita de arquivos: https://en.cppreference.com/w/cpp/io/basic_filebuf
- Tutoriais sobre expressões regulares em C++: https://www.cplusplus.com/reference/regex/
- Dicas para trabalhar com grandes arquivos em C++: https://stackoverflow.com/questions/3663373/reading-large-text-files-with-c