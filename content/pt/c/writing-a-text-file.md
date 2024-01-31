---
title:                "Escrevendo um arquivo de texto"
date:                  2024-01-19
html_title:           "Arduino: Escrevendo um arquivo de texto"
simple_title:         "Escrevendo um arquivo de texto"

category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c/writing-a-text-file.md"
---

{{< edit_this_page >}}

## O Quê & Porquê?
Escrever um arquivo de texto envolve a gravação de dados em um arquivo legível por humanos. Programadores fazem isso para salvar configurações, logs ou dados de saída para análises futuras.

## Como Fazer:
```C
#include <stdio.h>

int main() {
    FILE *arquivo = fopen("exemplo.txt", "w");

    if (arquivo == NULL) {
        printf("Erro ao abrir arquivo!\n");
        return 1;
    }

    fprintf(arquivo, "Olá, arquivo!\n");
    fprintf(arquivo, "Adeus, arquivo!\n");

    fclose(arquivo);

    return 0;
}
```
Saída esperada no arquivo `exemplo.txt`:
```
Olá, arquivo!
Adeus, arquivo!
```

## Mergulho Profundo:
Historicamente, a manipulação de arquivos em C é realizada através da biblioteca stdio.h, introduzida no início dos anos 70. Alternativas modernas incluem o uso de funções específicas do sistema operacional ou bibliotecas de terceiros para maior controle ou simplicidade. Detalhes de implementação incluem o uso de modos de abertura de arquivo, como `w` para escrita, e a necessidade de fechar o arquivo com `fclose()` para liberar recursos.

## Veja Também:
- Documentação oficial da GNU sobre biblioteca C: https://www.gnu.org/software/libc/manual/html_node/Output-Streams.html
- Tutorial detalhado sobre manipulação de arquivos em C: https://www.tutorialspoint.com/cprogramming/c_file_io.htm
- Referência sobre o `fprintf`: http://www.cplusplus.com/reference/cstdio/fprintf/
