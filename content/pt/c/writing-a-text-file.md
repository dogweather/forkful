---
title:                "Escrevendo um arquivo de texto"
html_title:           "C: Escrevendo um arquivo de texto"
simple_title:         "Escrevendo um arquivo de texto"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c/writing-a-text-file.md"
---

{{< edit_this_page >}}

## O que e Por que?

Escrever um arquivo de texto é simplesmente salvar informações em um arquivo de texto que pode ser lido por humanos, usando palavras e caracteres. Os programadores geralmente fazem isso para armazenar informações importantes ou resultados de suas aplicações.

## Como fazer:

Para escrever um arquivo de texto em C, você pode usar a função `fprintf()` do cabeçalho `stdio.h`. Veja um exemplo abaixo:

```C
#include <stdio.h>

int main()
{
    // Criando um ponteiro de arquivo
    FILE *arquivo;

    // Abrindo o arquivo com permissão de escrita
    arquivo = fopen("meu_arquivo.txt", "w");

    // Escrevendo no arquivo usando fprintf
    fprintf(arquivo, "Olá, este é um arquivo de texto criado pelo programa!\n");

    // Fechando o arquivo
    fclose(arquivo);

    return 0;
}
```

Isso criará um arquivo chamado "meu_arquivo.txt" no mesmo diretório do seu programa, e escreverá a mensagem "Olá, este é um arquivo de texto criado pelo programa!" nele.

## Mergulho Profundo:

Antes do padrão ANSI C, os programadores usavam a função `putc()` para escrever caracteres em um arquivo de texto. No entanto, com o surgimento da função `fprintf()`, tornou-se mais fácil escrever strings e variáveis formatadas em um arquivo. Além disso, outras linguagens de programação, como Python e Java, também possuem funções semelhantes para escrever em arquivos de texto.

## Veja também:

- [Manipulação de Arquivos em C](https://www.programiz.com/c-programming/c-file-input-output)
- [Tutorial C: Escrevendo em Arquivos de Texto](https://www.tutorialspoint.com/cprogramming/c_file_io.htm)
- [Documentação da função fprintf](https://www.tutorialspoint.com/c_standard_library/c_function_fprintf.htm)