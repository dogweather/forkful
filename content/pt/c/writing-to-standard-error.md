---
title:                "Escrevendo para o erro padrão"
html_title:           "C: Escrevendo para o erro padrão"
simple_title:         "Escrevendo para o erro padrão"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Por que

Escrever para o erro padrão (standard error) é uma forma importante de acompanhar o desempenho do seu programa e identificar possíveis problemas.

## Como Fazer

Para escrever para o erro padrão, utilize a função `fprintf()` e especifique o fluxo de saída como `stderr`, por exemplo:

```C
fprintf(stderr, "Mensagem de erro");
```

Isso irá imprimir a mensagem de erro no console ou terminal, dependendo do ambiente de execução.

Outra opção é utilizar a função `stderr()` que é equivalente a utilizar a função `fprintf()` com o fluxo de saída já definido como `stderr`:

```C
stderr("Mensagem de erro");
```

É importante lembrar de incluir o cabeçalho `stdio.h` no início do seu código para utilizar essas funções.

## Mergulho Profundo

O que é exatamente o erro padrão? É um fluxo de saída especial que é utilizado para imprimir mensagens de erro ou alerta relacionadas ao seu programa. Ao escrever para o erro padrão, você está redirecionando essas mensagens para um local específico, ao invés de misturá-las com a saída normal do seu programa.

Outra vantagem de escrever para o erro padrão é que ele é considerado um fluxo de saída não-tampado, o que significa que as mensagens serão exibidas imediatamente e não serão armazenadas em um buffer antes de serem impressas.

## Veja Também

- [Documentação oficial do C](https://devdocs.io/c/)
- [Como depurar código em C](https://betterprogramming.pub/how-to-debug-your-c-code-13d94493e2a7)
- [Depurando com o GDB](https://www.geeksforgeeks.org/gdb-command-in-c-with-examples/)