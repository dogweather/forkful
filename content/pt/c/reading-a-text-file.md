---
title:                "C: Lendo um arquivo de texto."
programming_language: "C"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Por que

Ler e manipular arquivos de texto é uma habilidade fundamental para qualquer programador. Ao dominar essa técnica, é possível criar programas que leiam e processem grandes quantidades de dados, tornando-os mais eficientes e dinâmicos. Portanto, se você quer aprimorar suas habilidades de programação, é essencial aprender como ler arquivos de texto.

## Como Fazer

Antes de começar, é importante entender que um arquivo de texto é simplesmente uma sequência de caracteres, que podem representar letras, números e símbolos. Em C, podemos usar a biblioteca padrão "stdio.h" para manipular arquivos de texto. Vamos ver um exemplo de como ler um arquivo de texto chamado "exemplo.txt":

```C
#include <stdio.h>

int main() {
    // Abrindo o arquivo
    FILE *arquivo = fopen("exemplo.txt", "r");

    // Verificando se o arquivo foi aberto corretamente
    if (arquivo == NULL) {
        printf("Erro ao abrir o arquivo!");
        return 1;
    }
  
    // Lendo o arquivo caractere por caractere
    char caractere;
    while ((caractere = fgetc(arquivo)) != EOF) {
        printf("%c", caractere); // Imprimindo o caractere lido
    }

    // Fechando o arquivo
    fclose(arquivo);
    return 0;
}
```

**Saída:**

```
Este é um exemplo de arquivo de texto.
Aqui temos várias linhas 
com diferentes tipos de caracteres: $, %, &, #, entre outros.
```

Nesse exemplo, usamos as funções `fopen()` e`fclose()` para abrir e fechar o arquivo, respectivamente. Já a função `fgetc()` é responsável por ler o arquivo caractere por caractere. Além disso, utilizamos a variável `caractere` para armazenar o valor do caractere lido a cada iteração. No final, imprimimos o caractere na tela usando a função `printf()`.

## Mergulho Profundo

Além da função `fgetc()`, a biblioteca "stdio.h" possui outras funções úteis para ler arquivos de texto:

- `fgets()` - lê uma linha do arquivo e armazena em uma string;
- `fscanf()` - lê dados formatados do arquivo;
- `fread()` - lê um bloco de dados do arquivo;
- `getline()` - lê uma linha do arquivo dinamicamente alocada.

É importante lembrar de sempre verificar se o arquivo foi aberto corretamente e de fechá-lo após o uso, para evitar erros e vazamentos de memória. Além disso, é possível abrir o arquivo em diferentes modos (`"r"`, `"w"`, `"a"`, entre outros), dependendo da forma como você quer ler ou escrever dados no arquivo.

## Veja Também

- [Documentação da biblioteca "stdio.h"](https://www.cplusplus.com/reference/cstdio/)
- [Tutorial em vídeo de como ler e escrever arquivos de texto em C](https://www.youtube.com/watch?v=I-hZkUa9mIs)
- [Exemplo de programa que lê e escreve em um arquivo de texto em C](https://github.com/danielschmitz/c-file-operations-example)