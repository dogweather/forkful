---
title:                "Criando um arquivo temporário"
html_title:           "C++: Criando um arquivo temporário"
simple_title:         "Criando um arquivo temporário"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/cpp/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## O que e Por que?

Criar um arquivo temporário, também conhecido como "temp file" ou "temporary file", é uma prática comum na programação. Isso envolve a criação de um arquivo que será usado temporariamente durante a execução de um programa. Programadores geralmente fazem isso para armazenar dados temporários ou para realizar operações que requerem a criação de um arquivo.

## Como fazer:

```C++
#include <iostream>
#include <fstream> // biblioteca para manipulação de arquivos

int main() {
  // criar um arquivo temporário usando o comando tempnam()
  char *nome_arquivo = tempnam(NULL, "tmp"); // NULL indica que pode ser criado em qualquer diretório, "tmp" é o prefixo do nome do arquivo
  std::ofstream arquivo(nome_arquivo); // cria o arquivo

  // escrever conteúdo no arquivo temporário
  arquivo << "Este é um arquivo temporário!" << std::endl;

  // imprimir o nome do arquivo criado
  std::cout << "Arquivo temporário criado: " << nome_arquivo << std::endl;

  // fechar e remover o arquivo temporário
  arquivo.close();
  remove(nome_arquivo);

  return 0;
}
```

Output:
```
Arquivo temporário criado: tmp/temp
```

## Mergulho Profundo:

Criar arquivos temporários é uma prática antiga na programação, que remonta à época dos sistemas operacionais Unix. Antigamente, os programadores usavam a função tmpnam() para criar arquivos temporários, mas essa função se tornou obsoleta devido a problemas de segurança e confiabilidade.

Existem algumas alternativas para criar arquivos temporários, como a função tmpfile() ou a biblioteca Boost. No entanto, o método mostrado acima com o uso do comando tempnam() é o mais simples e amplamente utilizado.

Na implementação do comando tempnam(), o primeiro argumento recebe o diretório em que o arquivo temporário será criado. Se for passado como NULL, o sistema operacional escolherá um diretório adequado por padrão. O segundo argumento é o prefixo do nome do arquivo temporário, que pode ser especificado pelo usuário. Isso permite uma melhor organização e identificação de arquivos temporários quando um programa cria vários deles.

## Veja também:

- [Documentação oficial do comando tempnam()](https://www.cplusplus.com/reference/cstdio/tempnam/)
- [Outras maneiras de criar arquivos temporários em C++](https://www.geeksforgeeks.org/tmp-file-functions-in-c-c/)