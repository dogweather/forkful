---
title:                "C: Lendo argumentos da linha de comando"
programming_language: "C"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Por que ler argumentos da linha de comando?

Ler argumentos da linha de comando em um programa em C pode trazer muitos benefícios. Com essa funcionalidade, você pode criar programas mais interativos e personalizados, que permitem ao usuário fornecer informações específicas ao executá-los. Além disso, com argumentos da linha de comando, você pode automatizar tarefas e economizar tempo.

## Como ler argumentos da linha de comando em C

A leitura de argumentos da linha de comando em C é uma tarefa bastante simples. Tudo o que você precisa fazer é declarar os parâmetros "argc" e "argv" na sua função "main". O parâmetro "argc" representa o número de argumentos passados na linha de comando e "argv" é um array de strings contendo cada um desses argumentos.

Para acessar os argumentos individuais, basta usar o índice do array "argv". Por exemplo, se o primeiro argumento for o nome do seu programa, ele estará armazenado em "argv[0]". Você pode usar um loop "for" para percorrer todos os argumentos e imprimi-los na tela.

Veja um exemplo de código abaixo:

```C
#include <stdio.h>

int main(int argc, char *argv[]) {

    // imprime o nome do programa
    printf("Nome do programa: %s\n", argv[0]);

    // imprime todos os argumentos
    for (int i = 1; i < argc; i++) {
        printf("Argumento %d: %s\n", i, argv[i]);
    }

    return 0;
}
```

Se executarmos esse programa com o seguinte comando na linha de comando:

```
./programa arg1 arg2 arg3
```

O output será:

```
Nome do programa: ./programa
Argumento 1: arg1
Argumento 2: arg2
Argumento 3: arg3
```

É importante ressaltar que os argumentos da linha de comando são sempre lidos como strings, mesmo que sejam números. Portanto, é necessário fazer a conversão para o tipo de dado correto, caso seja necessário usá-los em operações matemáticas.

## Uma olhada mais profunda na leitura de argumentos da linha de comando

Além do número e da string do argumento, a função "main" também pode ter um terceiro parâmetro opcional, "envp", que contém as variáveis de ambiente do sistema. Isso pode ser útil em algumas situações, como a procura de um valor específico para um determinado ambiente.

Outro recurso interessante é a possibilidade de usar a biblioteca "getopt" para lidar com a leitura de argumentos opcionais. Com essa biblioteca, você pode definir opções com argumentos e definir o comportamento caso o usuário não forneça esses argumentos.

## Veja também

- [Tutorial sobre leitura de argumentos da linha de comando em C](https://www.guru99.com/c-command-line-arguments.html)
- [Documentação oficial do GNU getopt](https://www.gnu.org/software/libc/manual/html_node/Using-Getopt.html)