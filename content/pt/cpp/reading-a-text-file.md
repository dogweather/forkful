---
title:                "C++: Lendo um arquivo de texto"
programming_language: "C++"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/cpp/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Por que ler um arquivo de texto?

Ler um arquivo de texto é uma tarefa comum e essencial em muitos programas de computador. Através da leitura de um arquivo de texto, é possível acessar informações importantes e utilizá-las em diferentes partes do programa.

## Como fazer isso em C++?

Em C++, existem várias maneiras de ler um arquivo de texto. Uma das formas mais simples é utilizando a biblioteca `fstream`, que permite abrir, ler e fechar arquivos de forma eficiente. Veja um exemplo prático abaixo:

```C++
#include <fstream>
#include <iostream>

using namespace std;

int main() {
    // Abre o arquivo para leitura
    ifstream arquivo("arquivo.txt");

    // Verifica se o arquivo foi aberto corretamente
    if (!arquivo) {
        cerr << "Erro ao abrir o arquivo";
        return 1;
    }

    // Lê e imprime o conteúdo do arquivo linha por linha
    string linha;
    while (getline(arquivo, linha)) {
        cout << linha << endl;
    }

    // Fecha o arquivo
    arquivo.close();

    return 0;
}
```

A saída para um arquivo de texto com o seguinte conteúdo:

```
Primeira linha
Segunda linha
Terceira linha
```

será:

```
Primeira linha
Segunda linha
Terceira linha
```

## Uma olhada mais profunda

Além da leitura básica de um arquivo de texto, é possível realizar várias outras operações, como escrever no arquivo, buscar por determinadas informações, entre outros. Para isso, é importante conhecer outros conceitos e funções da linguagem C++. Vale a pena explorar a documentação e realizar experimentos para aprimorar suas habilidades de manipulação de arquivos de texto.

## Veja também

- [Documentação da biblioteca `fstream` em C++](https://www.cplusplus.com/reference/fstream/)
- [Exemplo de leitura de arquivo de texto em C++](https://www.geeksforgeeks.org/file-handling-c-classes/)
- [Tutorial completo sobre manipulação de arquivos em C++](https://www.tutorialspoint.com/cplusplus/cpp_files_streams.htm)