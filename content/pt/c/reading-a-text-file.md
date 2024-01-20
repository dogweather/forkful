---
title:                "Lendo um arquivo de texto"
html_title:           "Bash: Lendo um arquivo de texto"
simple_title:         "Lendo um arquivo de texto"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c/reading-a-text-file.md"
---

{{< edit_this_page >}}

## O quê e Por quê?

A leitura de um arquivo de texto em C envolve acessar e interpretar o conteúdo dentro de um arquivo `.txt`. Programadores precisam fazer isto para manipular dados, armazenar informações ou para fins de depuração.

## Como fazer:

```C
#include <stdio.h>

int main() {
    FILE *file = fopen("arquivo.txt", "r");
    char ch;
    if (file == NULL){
        printf("Não foi possível abrir o arquivo\n");
        return 1;
    }
    while((ch = fgetc(file)) != EOF){
        printf("%c", ch);
    }
    fclose(file);
    return 0;
}
```
Ao executar o código acima, ele irá ler e imprimir todo o conteúdo do `arquivo.txt`. Se o arquivo não puder ser aberto, o programa exibirá uma mensagem de erro.

## Mergulhando Fundo:

A função `fopen()` foi introduzida no C em 1972 e é um pilar para a leitura e escrita de arquivos desde então. Mas existem alternativas, como `fread()` e `fwrite()`, que permitem um controle maior sobre o fluxo de dados.

A leitura de um arquivo é normalmente feita caractere por caractere, mas o C nos permite ler linhas inteiras de uma vez usando funções como `fgets()`. Quando um arquivo é aberto com `fopen()`, um "stream" de arquivo é criado, e podemos ler ou gravar dados nesta "stream" até que seja fechada com `fclose()`.

Mas atenção – manipular arquivos requer um tratamento adequado de erros. Um programa precisa verificar se o arquivo foi aberto corretamente, caso contrário, operações de leitura ou gravação resultarão em comportamento indefinido.

## Veja também:

1. Para uma visão mais aprofundada sobre a manipulação de arquivos em C, visite: www.learn-c.org/en/File_IO
2. Para aprender outras maneiras de ler arquivos de texto em C, consulte: www.cprogramming.com/tutorial/cfileio.
3. Para uma explicação detalhada das funções de arquivo em C, veja: www.cplusplus.com/reference/cstdio/