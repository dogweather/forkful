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

## Por que imprimir saída de depuração em C?

Se você já teve que identificar e corrigir um bug em um programa escrito em C, provavelmente já percebeu que a tarefa pode ser muito desafiadora. Felizmente, a impressão de saída de depuração pode ser uma ferramenta muito útil nesses momentos.

## Como fazer?

A impressão de saída de depuração, também conhecida como "imprimir para console" ou "imprimir para stdout" pode ser realizada usando a função `printf()`. É importante notar que, para que a saída de depuração seja realmente útil, ela deve ser estrategicamente colocada em pontos específicos do código, como antes e depois de um trecho problemático.

```C
#include <stdio.h>

int main(){
  int n = 10;

  //imprimindo o valor de uma variável
  printf("O valor de n é: %d\n", n);

  //imprimindo uma mensagem de depuração
  printf("Aqui começa o loop...\n");

  //imprimindo valores dentro de um loop
  for(int i = 0; i < n; i++){
    printf("Iteração %d\n", i);
  }

  //imprimindo uma mensagem de fim de loop
  printf("Fim do loop!\n");

  return 0;
}
```

A saída desse programa seria:

```
O valor de n é: 10
Aqui começa o loop...
Iteração 1
Iteração 2
Iteração 3
Iteração 4
Iteração 5
Iteração 6
Iteração 7
Iteração 8
Iteração 9
Fim do loop!
```

## Mergulho profundo

Ao imprimir saída de depuração em diferentes pontos do código, é possível obter uma visão mais clara de como o programa está se comportando e identificar possíveis erros. Além disso, é possível usar a função `printf()` para formatar a saída de forma a facilitar a leitura e compreensão dos valores.

Por exemplo, além de simplesmente imprimir o valor de uma variável, é possível incluir informações adicionais, como o nome da variável ou até mesmo uma mensagem de erro. Isso pode tornar o processo de depuração mais rápido e eficiente.

Outro ponto importante a ressaltar é que, após resolver o bug e corrigir o código, é importante remover todas as impressões de saída de depuração. Isso ajuda a manter o código limpo e organizado.

## Veja também

- [Tutorial de C (em português)](https://www.cprogressivo.net/p/tutoriais-de-c.html)
- [Documentação oficial da função `printf()`](https://www.cplusplus.com/reference/cstdio/printf/)
- [Artigo sobre técnicas de depuração em C](https://www.educative.io/edpresso/what-is-debugging-in-c)