---
title:                "Lendo um arquivo de texto"
date:                  2024-01-20T17:53:54.149677-07:00
model:                 gpt-4-1106-preview
simple_title:         "Lendo um arquivo de texto"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c/reading-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why?
Ler um arquivo de texto em C é pegar dados de um arquivo guardado no seu disco para usar no seu programa. Fazemos isso porque muitas vezes os dados que precisamos estão armazenados em arquivos externos - configurações, informações que o usuário salvou, ou grandes volumes de dados para processamento.

## How to:
```C
#include <stdio.h>

int main() {
    FILE *arquivo;
    char linha[256]; // supõe que uma linha inteira cabe aqui
    
    arquivo = fopen("meu_arquivo.txt", "r"); // Abre para leitura
    
    if (arquivo == NULL) {
        perror("Erro ao abrir o arquivo");
        return 1;
    }
    
    while (fgets(linha, sizeof(linha), arquivo)) {
        printf("%s", linha);
    }
    
    fclose(arquivo); // Sempre feche o arquivo quando terminar
    return 0;
}
```
**Saída Exemplo:**
```
Primeira linha do arquivo.
Segunda linha do arquivo.
```

## Deep Dive
Ler arquivos de texto é uma das práticas mais antigas da programação. Desde os primeiros sistemas operacionais, os programas precisavam de uma maneira de ler e escrever dados persistentes. Em C, isso é feito através de um tipo `FILE` encontrado na biblioteca `stdio.h`, que representa um stream de arquivo.

Alternativas para a `fgets` incluem `fscanf`, para ler formatos específicos, e `fread` para ler blocos de bytes, útil para arquivos que não são somente de texto. 

Quando um arquivo é aberto com `fopen`, o sistema operacional cria um buffer de E/S para o stream, que é usado pela `fgets` para ler de forma eficiente. A função `fclose` finaliza o stream, libera o buffer de E/S e assegura que todas as saídas pendentes para o arquivo sejam escritas.

## See Also
- [Documentação sobre manipulação de arquivos em C](https://en.cppreference.com/w/c/io)
- [Tutorial sobre E/S de arquivo em C do tutorialspoint](https://www.tutorialspoint.com/cprogramming/c_file_io.htm)
- [Exploração de buffers de E/S do Stack Overflow](https://stackoverflow.com/questions/16466670/what-is-file-buffering)