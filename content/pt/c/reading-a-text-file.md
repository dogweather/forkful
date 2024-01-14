---
title:                "C: Lendo um arquivo de texto"
simple_title:         "Lendo um arquivo de texto"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Por que ler um arquivo de texto em C?

Ler arquivos de texto em C é uma habilidade muito importante para qualquer programador. Isso permite que você acesse e manipule dados armazenados em um arquivo e use esses dados em seus programas. Sem essa habilidade, sua capacidade de trabalhar com dados externos fica limitada.

## Como fazer isso:

Abaixo estão alguns exemplos de código em C que mostram como ler um arquivo de texto e imprimir seu conteúdo na tela:

```C
#include <stdio.h>

int main(){
    FILE *arquivo;
    char linha[50];

    arquivo = fopen("exemplo.txt", "r"); // abre o arquivo em modo de leitura (r)

    // lê cada linha do arquivo e imprime na tela
    while (fgets(linha, 50, arquivo) != NULL){
        printf("%s", linha);
    }

    fclose(arquivo); // fecha o arquivo
    return 0;
}
```

Exemplo de conteúdo do arquivo "exemplo.txt":

```
Essa é uma linha de texto.
Esta é outra linha.
E aqui está mais uma.
```

Output do programa:

```
Essa é uma linha de texto.
Esta é outra linha.
E aqui está mais uma.
```

## Aprofundando ainda mais:

Existem várias funções em C que permitem ler e manipular arquivos de texto. Aqui estão algumas delas:

- `fopen()`: abre um arquivo
- `fclose()`: fecha o arquivo
- `fgets()`: lê uma linha de texto do arquivo
- `fscanf()`: lê dados formatados do arquivo
- `fputc()`: escreve um caractere no arquivo
- `fprintf()`: escreve uma string formatada no arquivo

Além disso, existem várias opções de modo de abertura do arquivo, como `r` (somente leitura), `w` (escrita) e `a` (anexação).

Ler e entender a documentação dessas funções pode ser muito útil ao trabalhar com arquivos de texto em C.

## Veja também:

- [Tutorialspoint - leitura de arquivos em C](https://www.tutorialspoint.com/cprogramming/c_file_io.htm)
- [Programiz - manipulação de arquivos em C](https://www.programiz.com/c-programming/c-file-input-output)
- [GeeksforGeeks - manipulação de arquivos em C](https://www.geeksforgeeks.org/basics-file-handling-c/)