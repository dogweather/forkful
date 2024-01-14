---
title:    "C: Imprimindo saída de depuração"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/c/printing-debug-output.md"
---

{{< edit_this_page >}}

## Por que imprimir saída de debug em C?

Depurar um programa pode ser uma tarefa desafiadora, especialmente quando se lida com erros desconhecidos ou comportamentos inesperados. Nesses casos, imprimir saída de debug pode ser uma ferramenta valiosa para entender o fluxo do programa e identificar onde está o problema. Isso é útil tanto para desenvolvedores iniciantes quanto para profissionais experientes que desejam economizar tempo durante o processo de depuração.

## Como fazer isso em C?

Para imprimir saída de debug em C, podemos utilizar a função `printf` da biblioteca padrão `stdio.h`. Essa função aceita um ou mais parâmetros entre as aspas e os imprime na tela. Dessa forma, podemos inserir mensagens explicativas ou até mesmo variáveis durante a execução do programa.

```C
#include <stdio.h>

int main() {
  int x = 5;
  printf("O valor de x é %d", x);
  return 0;
}
```

Nesse exemplo, a mensagem "O valor de x é 5" será impressa na tela durante a execução do programa.

## Aprofundando-se em saída de debug

Além da função `printf`, existem outras maneiras de imprimir saída de debug em C, como a função `puts` e `write` da mesma biblioteca. Além disso, também é possível utilizar diretivas de pré-processador, como `#define`, para criar macros que facilitam a impressão de mensagens de debug.

Outra técnica útil é utilizar a função `fprintf` para imprimir em um arquivo ao invés de na tela. Isso pode ser útil quando se deseja fazer debug em um programa que está sendo executado em um ambiente sem terminal, como em sistemas embarcados.

No entanto, é importante lembrar de remover todas as mensagens de debug antes de finalizar o programa, já que elas podem impactar na performance e ocupar espaço desnecessariamente.

## Veja também

- [Tutorial de depuração em C](https://www.freecodecamp.org/news/printing-debug-output-in-c/)
- [Documentação da função printf](https://www.cplusplus.com/reference/cstdio/printf/)
- [Como utilizar macros para debug em C](https://www.geeksforgeeks.org/how-to-describe-a-macro-in-cgb-run-time-debug/)