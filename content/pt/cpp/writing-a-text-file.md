---
title:                "Escrevendo um arquivo de texto"
html_title:           "C++: Escrevendo um arquivo de texto"
simple_title:         "Escrevendo um arquivo de texto"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/cpp/writing-a-text-file.md"
---

{{< edit_this_page >}}

# O que e por que?

Escrever um arquivo de texto é simplesmente criar um arquivo que contém texto em seu conteúdo. Programadores frequentemente fazem isso para armazenar informações ou dados que possam ser usados ​​mais tarde no programa.

# Como fazer:

```C++
#include <iostream>
#include <fstream>

using namespace std;

int main()
{
  // Criando e abrindo um arquivo de texto chamado "arquivo.txt"
  ofstream arquivo("arquivo.txt");

  // Checando se o arquivo foi criado com sucesso
  if (arquivo.is_open())
  {
    // Escrevendo informações no arquivo
    arquivo << "Este é um arquivo de texto!";

    // Fechando o arquivo
    arquivo.close();

    // Imprimindo a mensagem de sucesso
    cout << "Arquivo de texto criado com sucesso!";
  }
  else
  {
    // Imprimindo a mensagem de erro
    cout << "Não foi possível criar o arquivo.";
  }

  return 0;
}
```

Saída:
```
Arquivo de texto criado com sucesso! 
```

# Mergulho profundo:

## Histórico:

Escrever um arquivo de texto é uma tarefa básica em programação, usado desde as primeiras linguagens de programação. Antes da popularidade da internet, os programadores gravavam seus códigos e informações em arquivos de texto para serem usados ​​posteriormente.

## Alternativas:

Enquanto escrever um arquivo de texto é uma opção simples e amplamente utilizada, existem outras alternativas disponíveis, como escrever em um banco de dados ou em um arquivo binário. A escolha da melhor opção depende do contexto e dos requisitos do programa.

## Detalhes de implementação:

Quando um arquivo de texto é criado, ele é salvo em formato ASCII, que atribui um número único a cada caractere no teclado padrão. Esses números são armazenados como bytes no arquivo e podem ser lidos e interpretados posteriormente pelo programa.

# Veja também:

- [Manipulação de arquivos em C++](https://www.cplusplus.com/doc/tutorial/files/)