---
title:                "Lendo argumentos da linha de comando"
html_title:           "C: Lendo argumentos da linha de comando"
simple_title:         "Lendo argumentos da linha de comando"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Por que
Algumas vezes, ao executar um programa em C, é necessário passar informações específicas para que ele funcione corretamente. Ao utilizar argumentos de linha de comando, é possível passar essas informações diretamente quando o programa é executado.

## Como fazer
Para ler argumentos de linha de comando em um programa em C, primeiro é necessário incluir a biblioteca "stdio.h". Em seguida, utilizamos os parâmetros "argc" e "argv" na função "main". "argc" é um inteiro que representa o número de argumentos passados e "argv" é um array de strings que contém os argumentos em si. A partir disso, podemos utilizar um loop para percorrer todos os argumentos e realizar as ações necessárias.

```
#include <stdio.h>

int main(int argc, char *argv[])
{
    // loop para percorrer todos os argumentos
    for (int i = 0; i < argc; i++)
    {
        printf("Argumento %d: %s\n", i, argv[i]);
    }

    return 0;
}
```
Exemplo de entrada:
```
./programa argumento1 argumento2 argumento3
```
Saída:
```
Argumento 0: ./programa
Argumento 1: argumento1
Argumento 2: argumento2
Argumento 3: argumento3
```

## Mergulho profundo
É importante lembrar que o primeiro argumento da lista "argv" sempre será o nome do programa executado. Além disso, os argumentos são passados como strings, portanto, é necessário realizar conversões para outros tipos de dados, se necessário.

Para facilitar o processo de leitura dos argumentos, podemos utilizar a função "getopt" da biblioteca "unistd.h". Essa função permite que especifiquemos as opções e argumentos que o programa espera receber. Além disso, ela também lida com as diferentes possibilidades de como os argumentos podem ser passados (por exemplo, com ou sem o uso de hífen).

## Veja também
- [Tutorial de argumentos de linha de comando em C](https://www.gnu.org/software/libc/manual/html_node/Example-of-Getopt.html)
- [Documentação da função "getopt" (em inglês)](https://man7.org/linux/man-pages/man3/getopt.3.html)