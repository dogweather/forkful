---
title:                "Lendo argumentos de linha de comando"
html_title:           "Arduino: Lendo argumentos de linha de comando"
simple_title:         "Lendo argumentos de linha de comando"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## O que é e Por quê?

Parâmetros de linha de comando são argumentos fornecidos ao programa durante a sua inicialização. Eles são amplamente utilizados para permitir opções personalizadas e ações, tornando nossos programas mais flexíveis e poderosos.

## Como Fazer:

Os argumentos de linha de comando em C são acessados via argumentos para a função `main()`:

```C
int main(int argc, char *argv[]) { ... }
```

Onde `argc` é o número de argumentos e `argv` é um array com as cadeias de caracteres de cada argumento.

Aqui está um exemplo simples de como lidar com argumentos de linha de comando:

```C
#include <stdio.h>

int main(int argc, char *argv[]) {
    int contador;
    printf("\nNúmero de argumentos: %d", argc);
    for (contador=0; contador<argc; contador++) {
        printf("\nargv[%d]: %s", contador, argv[contador]);
    }
    return 0;
}
```

A saída do exemplo acima, se executado com o comando `./prog arg1 arg2 arg3`, será:

```
Número de argumentos: 4
argv[0]: ./prog
argv[1]: arg1
argv[2]: arg2
argv[3]: arg3
```

## Um Mergulho Profundo

Historicamente, a leitura de argumentos de linha de comando tem feito parte de muitos programas Unix e C desde os primórdios da computação. Isso permite mais interatividade e flexibilidade no uso de programas.

Existem bibliotecas disponíveis, como a libgengetopt, que podem gerar código para lidar com argumentos de linha de comando de forma mais fácil e tais bibliotecas geralmente vêm com recursos adicionais, tais como a geração de ajuda e mensagens de erro.

A função `main()` em C é o ponto de entrada para qualquer aplicativo C. A declaração dos parâmetros `int argc, char *argv[]` permite ao programa aceitar argumentos de linha de comando do usuário. Quando o programa é iniciado, o sistema operacional passa esses argumentos para o programa.

## Veja Também

Segue abaixo alguns links úteis para aprofundar o estudo:

- [Documentação do GNU: Parsing Program Arguments](https://www.gnu.org/software/libc/manual/html_node/Parsing-Program-Arguments.html)
- [Página do StackOverflow sobre argumentos de linha de comando](https://stackoverflow.com/questions/3024197/what-does-int-argc-char-argv-mean)
- [GeeksforGeeks: Command line arguments in C/C++](https://www.geeksforgeeks.org/command-line-arguments-in-c-cpp/)