---
title:                "C++: Criando um arquivo temporário"
programming_language: "C++"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/cpp/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Por que criar um arquivo temporário em C++?

Sempre que estamos trabalhando em um programa em C++, é comum que precisemos armazenar informações em arquivos. Isso pode ser para salvar dados temporários, fazer backups ou até mesmo criar logs para facilitar a depuração do código. A criação de arquivos temporários é uma técnica específica que pode ser muito útil em algumas situações. Neste artigo, vamos explorar por que e como criar um arquivo temporário em C++.

## Como criar um arquivo temporário em C++

Para criar um arquivo temporário em C++, precisamos incluir a biblioteca `fstream` e utilizar a função `tmpfile()`. Essa função irá gerar um arquivo temporário e retornar um ponteiro para seu descritor. A partir daí, podemos utilizar as funções de leitura e escrita de arquivos normalmente, usando o descritor temporário gerado pela função `tmpfile()`.

```C++
#include <fstream>

int main() {

    // cria um arquivo temporário e retorna um ponteiro para seu descritor
    FILE* tempFile = tmpfile();

    // escreve uma string no arquivo temporário
    fprintf(tempFile, "Olá, este é um arquivo temporário!");

    // fecha o arquivo temporário
    fclose(tempFile);

    return 0;
}
```

Ao executar o programa, não veremos nenhum arquivo criado no disco rígido. Isso acontece porque os arquivos temporários são armazenados apenas na memória RAM, tornando-os ideais para armazenar informações temporárias que não precisam ser salvas permanentemente.

## Detalhando a criação de arquivos temporários em C++

Além da função `tmpfile()`, existem outras opções para criar arquivos temporários em C++. Podemos utilizar também a função `tmpnam()` que irá gerar um nome para o arquivo temporário, mas não irá criá-lo automaticamente. Nesse caso, é necessário utilizar o nome gerado para criar o arquivo com a função `fopen()` e, em seguida, manipulá-lo normalmente.

Outra alternativa é a função `tempnam()` que permite especificar um diretório para a criação do arquivo temporário. Além disso, podemos utilizar a função `mkstemp()` que permite especificar o nome do arquivo temporário e cria automaticamente o arquivo especificado.

## Veja também

Para saber mais sobre a criação de arquivos temporários em C++, confira os seguintes links:
- [Documentação oficial da função `tmpfile()`](https://en.cppreference.com/w/cpp/io/c/tmpfile)
- [Exemplos de uso da função `tmpfile()`](https://www.geeksforgeeks.org/tmpfile-function-in-c/)
- [Tutorial sobre a criação de arquivos temporários em C++](https://www.guru99.com/cpp-file-reading-writing.html)