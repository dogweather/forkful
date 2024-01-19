---
title:                "Imprimindo saída de debug"
html_title:           "C#: Imprimindo saída de debug"
simple_title:         "Imprimindo saída de debug"
programming_language: "C++"
category:             "C++"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/cpp/printing-debug-output.md"
---

{{< edit_this_page >}}

## O Que é & Por Quê?

Imprimir a saída de debug é essencialmente o processo de usar a função 'cout', ou similar, para ver os dados ao executar um programa. Os programadores fazem isso para rastrear erros, verificar variáveis e entender melhor o fluxo de execução do código.

## Como Fazer:

Vamos usar `cout` como exemplo para imprimir a saída de depuração.
```C++
#include <iostream>

int main() {
    int x = 5;

    std::cout << "O valor de x é: " << x << std::endl;
    return 0;
}
```
Na saída, você verá:
```
O valor de x é: 5
```

## Mergulho Profundo

Historicamente, a impressão de saída de depuração tem sido uma ferramenta valiosa desde os primeiros dias de programação. Antes de ferramentas de depuração mais sofisticadas, era a principal maneira de entender o que estava acontecendo durante a execução de um programa.

Existem alternativas para `cout` no C++, como `printf`, mas `cout` é geralmente o preferido no C++ por ser mais flexível e seguro.

Os detalhes da implementação real do `cout` são um pouco complexos, pois fazem parte dos fluxos de E/S do C++, mas, no nível mais básico, ele apenas envia os dados para a saída padrão (normalmente sua tela).

## Ver Também

Para mais informações, veja os links abaixo:

- Documentação oficial do C++ sobre `cout`: http://www.cplusplus.com/reference/iostream/cout
- Mais maneiras de depurar no C++: https://isocpp.org/wiki/faq/debugging
- Alternativas para `cout`: https://www.learncpp.com/cpp-tutorial/more-on-printf-and-scanf