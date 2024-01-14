---
title:                "C: Escrevendo no erro padrão"
programming_language: "C"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Por que escrever no standard error?

Escrever no standard error é uma técnica fundamental na programação em C. Isso permite que mensagens de erro e depuração sejam impressas no terminal, ao invés de serem exibidas com o restante da saída normal do programa. Isso pode ajudar a identificar e corrigir erros com mais facilidade durante o processo de desenvolvimento.

## Como fazer?

Para imprimir mensagens no standard error, é necessário utilizar a função `fprintf()`, passando o valor `stderr` como primeiro parâmetro. Por exemplo:

```C
fprintf(stderr, "Erro: o valor inserido é inválido.");
```

Isso irá imprimir a mensagem "Erro: o valor inserido é inválido." no terminal. É importante notar que a formatação utilizada em `fprintf()` é a mesma utilizada em `printf()`.

Um exemplo mais prático seria o seguinte código, que pede ao usuário um número e imprime o dobro do número inserido no standard error:

```C
#include <stdio.h>

int main() {
    int num;
    printf("Insira um número: ");
    scanf("%d", &num);

    if (num > 0) {
        fprintf(stderr, "O dobro do número inserido é %d.", num * 2);
    } else {
        fprintf(stderr, "Erro: o número inserido é inválido.");
    }

    return 0;
}
```

A saída desse programa poderia ser:

```
Insira um número: 5
Erro: o número inserido é inválido.
```

## Dando um mergulho mais profundo

Além de simplesmente imprimir mensagens de erro, o standard error também possui outras funcionalidades importantes. Por exemplo, é possível utilizar `fprintf()` para direcionar a saída de um programa para um arquivo ao invés do terminal. Isso pode ser feito passando o nome do arquivo como primeiro parâmetro em vez de `stderr`.

Outra funcionalidade útil é a possibilidade de colorir as mensagens de erro, facilitando a identificação visual de erros específicos. Isso pode ser feito utilizando as sequências de escape ANSI em conjunto com `fprintf()`.

Veja um exemplo de como colorir a mensagem de erro em vermelho:

```C
fprintf(stderr, "\033[0;31mErro\033[0m: o valor inserido é inválido.");
```

Aqui, a sequência `\033[0;31m` irá mudar a cor da impressão seguinte para vermelho, e `\033[0m` irá restaurar a cor padrão, após a mensagem ser exibida.

## Veja também

- [Documentação oficial sobre o standard error em C](https://www.gnu.org/software/libc/manual/html_node/Standard-Error.html)
- [Explicação detalhada sobre o uso de `fprintf()` e `stderr`](https://www.geeksforgeeks.org/generating-error-unknown-id-fprintf-stderr/)
- [Exemplo de formatação de cores com sequências de escape ANSI](https://gist.github.com/rossdylan/0827c1e3baa149c1f667)