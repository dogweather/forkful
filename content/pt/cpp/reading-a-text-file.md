---
title:                "Lendo um arquivo de texto."
html_title:           "C++: Lendo um arquivo de texto."
simple_title:         "Lendo um arquivo de texto."
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/cpp/reading-a-text-file.md"
---

{{< edit_this_page >}}

## O que é e por que fazer?

Ler um arquivo de texto é uma tarefa comum para programadores que trabalham com C++. Isso significa acessar e exibir o conteúdo de um arquivo de texto, como um documento ou código fonte, em uma aplicação. É uma forma eficiente de obter informações armazenadas de forma persistente em um formato reconhecível.

## Como fazer:

Existem várias maneiras de ler um arquivo de texto em C++. Uma delas é usando a função `ifstream`, como mostrado no exemplo abaixo:

```C++
#include <iostream>
#include <fstream>
using namespace std;

int main() {
    string line;
    // abre um arquivo de texto chamado "exemplo.txt"
    ifstream arquivo("exemplo.txt");
    
    // verifica se o arquivo foi aberto corretamente
    if (arquivo.is_open()) {
        // loop que lê cada linha do arquivo
        while (getline(arquivo, line)) {
            // exibe a linha na tela
            cout << line << "\n";
        }
        // fecha o arquivo
        arquivo.close();
    }
    else {
        cout << "Não foi possível abrir o arquivo";
    }
    return 0;
}
```

Exemplo de arquivo de texto "exemplo.txt":

```
Lorem ipsum dolor sit amet
consectetur adipiscing elit
sed do eiusmod tempor incididunt
ut labore et dolore magna aliqua
```

Resultado na tela:

```
Lorem ipsum dolor sit amet
consectetur adipiscing elit
sed do eiusmod tempor incididunt
ut labore et dolore magna aliqua
```

## Mergulho Profundo:

Ler arquivos de texto em C++ tornou-se ainda mais fácil com a padronização da linguagem em 1994. Antes disso, as funções de leitura de arquivos eram implementadas de forma diferente em cada compilador. Agora, com o uso da biblioteca padrão, os programadores têm uma maneira uniforme de ler arquivos com a função `ifstream`.

Alternativamente, também é possível ler arquivos de texto usando a função `fopen()` e suas variantes, como `fgets()`. No entanto, isso requer a manipulação manual do ponteiro de arquivo, o que pode ser mais complicado.

Para implementar a leitura de arquivos de texto, o compilador usa a técnica de "streaming", que segue um fluxo de entrada constante para acessar o conteúdo do arquivo. Quando uma linha é lida, o ponteiro é movido para a próxima linha, permitindo a leitura sequencial do arquivo.

## Veja também:

- [Leitura e escrita de arquivos em C++](https://www.cplusplus.com/doc/tutorial/files/)
- [Função ifstream em C++](https://www.cplusplus.com/reference/fstream/ifstream/)