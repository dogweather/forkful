---
title:    "C: Lendo argumentos da linha de comando"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/c/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Por que ler argumentos da linha de comando é importante?

Ler argumentos da linha de comando é uma tarefa essencial para qualquer programador em C. É uma forma de fornecer informações adicionais para o programa ser executado sem a necessidade de alterar o código fonte. Além disso, pode ajudar a tornar o código mais flexível e reutilizável.

## Como ler argumentos da linha de comando em C

O primeiro passo para ler argumentos da linha de comando em C é incluir a biblioteca padrão `stdio.h`, que contém a função `main()`. Em seguida, é necessário declarar os parâmetros `argc` e `argv` na assinatura da função `main()`. O `argc` representa o número de argumentos passados na linha de comando, enquanto o `argv` é um vetor que contém os argumentos em si.

Dentro do bloco `main()`, podemos utilizar um loop `for` para percorrer o vetor `argv` e imprimir cada argumento individualmente, como mostrado no exemplo abaixo:

```C
#include <stdio.h>

int main(int argc, char *argv[]) {
    for (int i = 0; i < argc; i++) {
        printf("Argumento %d: %s \n", i, argv[i]);
    }
    return 0;
}
```

Ao executar esse código com os argumentos "apple", "banana" e "orange" na linha de comando, teremos o seguinte resultado:

```
Argumento 0: ./programa
Argumento 1: apple
Argumento 2: banana
Argumento 3: orange
```

Além disso, é possível utilizar funções como `strcmp()` ou `atoi()` para realizar operações com os argumentos lidos. Consulte a seção "Deep Dive" para mais informações sobre essas funções.

## Mergulho profundo em argumentos da linha de comando

Ao usar a função `strcmp()`, podemos comparar os argumentos com strings específicas para executar ações diferentes no programa. Por exemplo, se quisermos executar uma função específica apenas quando o primeiro argumento for "maçã", podemos fazer o seguinte:

```C
#include <stdio.h>
#include <string.h>

int main(int argc, char *argv[]) {
    if (strcmp(argv[1], "maçã") == 0) {
        // Executa uma função específica
    }
    return 0;
}
```

Já a função `atoi()` pode ser útil para converter argumentos que foram passados como strings para valores numéricos. Isso pode ser útil quando precisamos realizar cálculos com esses argumentos.

## Veja também

- [Documentação oficial do GCC sobre argumentos da linha de comando](https://gcc.gnu.org/onlinedocs/gcc-3.4.4/cpp/Initial-processing.html)
- [Tutorial em vídeo sobre como ler argumentos da linha de comando em C](https://www.youtube.com/watch?v=MH4nCJzWnsk)
- [Exemplos de utilização de argumentos da linha de comando em C](https://www.programiz.com/c-programming/c-command-line-arguments)