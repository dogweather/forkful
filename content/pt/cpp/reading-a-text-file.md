---
title:    "C++: Leitura de um arquivo de texto"
keywords: ["C++"]
---

{{< edit_this_page >}}

## Por que ler um arquivo de texto?

Muitas vezes, quando estamos programando em C++, é necessário lidar com arquivos de texto. Isso pode ser para ler informações, armazenar dados ou até mesmo para fins de depuração. Então, por que é importante saber como ler um arquivo de texto? Porque isso nos dá uma maior flexibilidade e nos possibilita trabalhar com diferentes tipos de dados em nossos programas.

## Como ler um arquivo de texto em C++

Para ler um arquivo de texto em C++, podemos usar as funções de entrada e saída padrão e a biblioteca <fstream>. Vamos dar uma olhada em um exemplo de código:

```C++
#include <iostream>
#include <fstream>

using namespace std;

int main() {

    // Criar um objeto de fluxo de entrada para abrir o arquivo
    ifstream inputFile("arquivo.txt");

    if (!inputFile) {
        // Verificar se o arquivo foi aberto corretamente
        cerr << "Não foi possível abrir o arquivo." << endl;
        return 1;
    }

    // Declarar uma variável para armazenar o conteúdo do arquivo
    string conteudo;

    // Ler o arquivo linha por linha
    while (getline(inputFile, conteudo)) {
        // Imprimir o conteúdo na tela
        cout << conteudo << endl;
    }

    // Fechar o arquivo
    inputFile.close();

    return 0;
}
```

Vamos explicar o código acima linha por linha. Primeiro, incluímos as bibliotecas necessárias e criamos um objeto de fluxo de entrada para abrir o arquivo desejado. Em seguida, verificamos se o arquivo foi aberto corretamente e, se não foi, encerramos o programa. Em seguida, declaramos uma variável para armazenar o conteúdo do arquivo e usamos um loop para ler o arquivo linha por linha e imprimi-lo na tela. Por fim, fechamos o arquivo e encerramos o programa.

## Mergulho profundo

Além de simplesmente ler um arquivo de texto, também podemos realizar outras operações, como gravar no arquivo ou ler e armazenar dados em variáveis. Também podemos usar funções adicionais, como a função <getline> para ler uma linha inteira e a função <get> para ler um caractere específico.

Outra coisa importante a ter em mente ao lidar com arquivos de texto é o formato utilizado. Dependendo do sistema operacional em que o arquivo foi criado, o formato pode ser diferente. Portanto, é importante verificar e adaptar o código, se necessário, para garantir que o conteúdo seja lido corretamente.

## Veja também

- Referência da biblioteca <fstream> (em inglês): https://www.cplusplus.com/reference/fstream/
- Tutorial de arquivos em C++ (em português): https://www.devmedia.com.br/arquivos-em-c-plus-plus/26531