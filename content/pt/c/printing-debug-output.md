---
title:                "Imprimindo saída de depuração"
html_title:           "C: Imprimindo saída de depuração"
simple_title:         "Imprimindo saída de depuração"
programming_language: "C"
category:             "C"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c/printing-debug-output.md"
---

{{< edit_this_page >}}

## O que é e por que fazemos isso?

Imprimir saída de depuração é uma técnica utilizada pelos programadores para ajudá-los a encontrar erros e bugs em seus códigos. Ao exibir informações específicas durante a execução do programa, os programadores podem identificar e corrigir problemas com mais eficiência.

## Como fazer:

Usar a função ```printf()``` é a maneira mais comum de imprimir saída de depuração em C. Ao adicionar a mensagem que você deseja exibir dentro das aspas após o primeiro parêntese, você pode exibir informações úteis durante a execução do programa. Por exemplo:

```C
int x = 5;
printf("O valor de x é: %d\n", x);
```

Este código irá imprimir a mensagem "O valor de x é: 5" no terminal. Usar o %d como especificador de formato indica que isso é um inteiro e o \n adiciona uma nova linha para tornar a saída mais legível.

## Mergulho profundo:

A impressão de saída de depuração tem sido uma técnica amplamente utilizada pelos programadores desde os primeiros dias da programação. Alternativas incluem o uso de ferramentas de depuração ou registradores de eventos. No entanto, imprimir saída de depuração pode ser útil porque permite que os programadores personalizem exatamente o que desejam exibir e o formato da saída.

A função ```printf()``` na verdade faz parte da biblioteca de funções padrão e usa o arquivo de cabeçalho ```stdio.h```. Isso significa que ela pode ser usada em qualquer programa C sem a necessidade de importar bibliotecas adicionais.

## Veja também:

- [Documentação oficial da função printf](https://www.cplusplus.com/reference/cstdio/printf/)
- [Guia de depuração em C](https://www.tutorialspoint.com/cprogramming/c_debugging.htm)
- [Técnicas avançadas de depuração em C](https://www.techpursue.com/debugging-c-programming/)