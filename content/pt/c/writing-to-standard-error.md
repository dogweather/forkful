---
title:                "C: Escrevendo para o erro padrão."
simple_title:         "Escrevendo para o erro padrão."
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Por que escrever para "standard error" em C?

Escrever para "standard error" é uma forma importante de depurar e comunicar informações de erro durante a execução de um programa em C. Ao escrever para "standard error", os programadores podem fornecer informações detalhadas sobre o que aconteceu durante a execução do programa, facilitando a identificação e correção de possíveis erros.

## Como fazer isso em C

A escrita para "standard error" em C é feita utilizando a função `fprintf`. Esta função permite ao programador especificar qual é o fluxo de escrita, neste caso, o "standard error", e também permite o uso de formatação, tornando a mensagem de erro mais legível.

Abaixo está um exemplo de código em C que escreve uma mensagem de erro para o "standard error":

```C
#include <stdio.h>

int main() {
    int divisor = 0;
    int resultado;

    if (divisor == 0) {
        fprintf(stderr, "Não é possível dividir por zero.");
    } else {
        resultado = 10 / divisor;
        printf("Resultado: %d", resultado);
    }

    return 0;
}
```

Este código verifica se o divisor é igual a zero e, caso seja, escreve uma mensagem de erro para o "standard error". Se o divisor for diferente de zero, o código continuará a execução normalmente.

O resultado do código acima será:

```
Não é possível dividir por zero.
```

## Mais informações sobre escrever para "standard error"

Escrever para "standard error" pode ser especialmente útil em situações em que o "standard output" não está disponível, como em aplicações que rodam como serviços de sistema. Além disso, a escrita para "standard error" pode ser facilmente redirecionada para um arquivo de log, permitindo que os programadores identifiquem e corrijam erros em aplicações em produção.

Outro ponto importante a ser lembrado é que mensagens de erro para o "standard error" devem ser claras e informativas, facilitando a compreensão do que aconteceu durante a execução do programa. Ao utilizar a formatação de `fprintf`, os programadores podem inserir variáveis e informações específicas sobre o erro, tornando a mensagem ainda mais útil para a depuração.

## Veja também

- [Documentação da função fprintf em C](https://www.cplusplus.com/reference/cstdio/fprintf/)
- [Tutorial sobre escrever para "standard error"](https://www.linuxjournal.com/article/5831)
- [Perguntas frequentes sobre o uso de "standard error" em C](https://www.c-faq.com/stdio/stderr.html)