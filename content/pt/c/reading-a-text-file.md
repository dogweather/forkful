---
title:                "Lendo um arquivo de texto"
html_title:           "C: Lendo um arquivo de texto"
simple_title:         "Lendo um arquivo de texto"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c/reading-a-text-file.md"
---

{{< edit_this_page >}}

## O que & Porquê?

A leitura de um arquivo de texto é uma tarefa comum e útil para os programadores. Basicamente, se trata de abrir e ler o conteúdo de um arquivo de texto, podendo ser utilizado para ler diferentes tipos de dados, como strings, números e até mesmo estruturas de dados mais complexas. A razão pela qual os programadores fazem isso é para processar e manipular esses dados em seus programas.

## Como fazer:

Aqui estão dois exemplos simples de como ler um arquivo de texto em C:

```
#include <stdio.h>

int main() {
  FILE *arquivo;
  char linha[100];

  arquivo = fopen("texto.txt", "r"); // abre o arquivo em modo de leitura

  while(fgets(linha, 100, arquivo) != NULL) { // lê cada linha do arquivo e armazena na variável "linha"
    printf("%s", linha); // imprime a linha na tela
  }

  fclose(arquivo); // fecha o arquivo
}
```

Ao executar esse código, o programa lerá o arquivo "texto.txt" e imprimirá seu conteúdo na tela. Note que é necessário utilizar a função "fclose" para fechar o arquivo após a leitura.

Outra maneira de ler um arquivo de texto é utilizando a função "fscanf":

```
#include <stdio.h>

int main() {
  FILE *arquivo;
  char palavra[20];

  arquivo = fopen("palavras.txt", "r"); // abre o arquivo em modo de leitura

  while(fscanf(arquivo, "%s", palavra) != EOF) { // lê uma palavra por vez e armazena em "palavra"
    printf("%s\n", palavra); // imprime a palavra na tela
  }

  fclose(arquivo); // fecha o arquivo
}
```

Neste exemplo, o programa lê cada palavra separadamente do arquivo "palavras.txt" e imprime na tela, até que chegue ao final do arquivo (EOF - end of file).

## Mergulho Profundo:

Ler arquivos de texto remonta aos primeiros dias da programação. Antes do desenvolvimento de formatos de dados mais complexos, como bancos de dados e arquivos binários, os programadores usavam arquivos de texto simples para armazenar e manipular dados. Hoje em dia, ainda é uma técnica amplamente utilizada, especialmente para leitura de configurações e arquivos de log.

Além da função "fopen", que utilizamos nos exemplos anteriores, também existem outras funções úteis para ler e manipular arquivos, como "fprintf", "fputc" e "fread". Além disso, há também a possibilidade de utilizar bibliotecas externas, como a "libfread", para facilitar o processo de leitura de arquivos.

## Veja Também:

- [The C File Input/Output Tutorial](https://www.programiz.com/c-programming/c-file-input-output)
- [fopen() function in C](https://www.geeksforgeeks.org/fopen-for-an-existing-file-in-write-mode/)
- [C Programming: Reading and Writing Files](https://www.thoughtco.com/reading-and-writing-files-in-c-program-4022383)