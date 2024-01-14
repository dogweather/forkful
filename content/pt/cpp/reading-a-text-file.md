---
title:                "C++: Lendo um arquivo de texto"
simple_title:         "Lendo um arquivo de texto"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/cpp/reading-a-text-file.md"
---

{{< edit_this_page >}}

# Por que ler um arquivo de texto no C++

Ler arquivos de texto é uma habilidade básica e valiosa para qualquer programador de C++. Isso permite que você acesse e manipule grandes quantidades de dados armazenados em um arquivo de texto, o que é útil em muitas aplicações.

## Como fazer isso

Para ler um arquivo de texto no C++, é necessário seguir alguns passos simples:

```
#include <iostream>
#include <fstream>

using namespace std;

int main() {
  ifstream file;
  file.open("arquivo.txt");

  if (!file) {
    cout << "Erro ao abrir o arquivo.";
    return 1;
  }

  // lê o arquivo e exibe cada linha na tela
  string linha;
  while (getline(file, linha)) {
    cout << linha << endl;
  }

  file.close(); // fecha o arquivo

  return 0;
}
```

Assim, o arquivo "arquivo.txt" será aberto e lido linha por linha, até o fim do arquivo. A função ```getline()``` é responsável por armazenar cada linha em uma string.

O código acima é apenas um exemplo básico, mas existem muitas outras formas de ler e manipular arquivos de texto no C++. Vale a pena explorar diferentes métodos e escolher o que melhor se adequa à sua necessidade.

## Aprofundando mais

Além de apenas ler e exibir o conteúdo do arquivo, é possível realizar muitas outras operações com essa informação. Por exemplo, é possível separar cada linha em diferentes variáveis ​​para trabalhar com dados específicos.

Também é importante lembrar de sempre fechar o arquivo quando terminar de utilizá-lo, para evitar erros e problemas no futuro.

## Veja também

- [Introdução a Arquivos](https://www.cplusplus.com/doc/tutorial/files/)
- [Manipulação de arquivos em C++](https://www.devmedia.com.br/manipulacao-de-arquivos-em-c/36643)
- [C++ File Handling](https://www.geeksforgeeks.org/file-handling-c-classes/)