---
title:                "Imprimindo saída de depuração"
html_title:           "C++: Imprimindo saída de depuração"
simple_title:         "Imprimindo saída de depuração"
programming_language: "C++"
category:             "C++"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/cpp/printing-debug-output.md"
---

{{< edit_this_page >}}

# O que é e por que os programadores usam a saída de depuração

A saída de depuração é uma técnica usada por programadores para imprimir informação valiosa durante a execução de um programa, a fim de identificar possíveis erros e problemas. Isso permite que o programador veja o estado do programa em diferentes pontos e rastreie o fluxo de dados. É uma ferramenta essencial na caixa de ferramentas de um programador e pode ajudar a economizar tempo na identificação e resolução de problemas.

# Como fazer

A saída de depuração é facilmente implementada em C++ usando o comando `std::cout`. Simplesmente inclua a biblioteca `iostream` e use o seguinte código:

```C++
#include <iostream>

// Seu código aqui

std::cout << "Mensagem de depuração!" << std::endl;
```

Você também pode imprimir variáveis adicionando-as ao comando `std::cout`:

```C++
int variavel = 5;

std::cout << "O valor da variável é: " << variavel << std::endl;
```

Isso imprimirá a mensagem "O valor da variável é: 5" na saída de texto do seu programa.

# Detalhes mais profundos

A saída de depuração tem sido usada por programadores desde os primeiros dias da programação. Antes da era das ferramentas de depuração modernas, a impressão de mensagens de depuração era a única maneira de identificar e corrigir bugs em programas. Hoje em dia, existem ferramentas mais sofisticadas disponíveis para depuração, como depuradores e registradores de eventos, mas a saída de depuração ainda é uma técnica valiosa e amplamente utilizada.

Existem diferentes maneiras de implementar a saída de depuração em C++, incluindo usar a função `printf` da biblioteca `cstdio` e criar suas próprias funções de depuração personalizadas. No entanto, o `std::cout` é a abordagem mais comum e fácil de usar, especialmente para iniciantes.

# Veja também

- [Ferramentas de depuração em C++](https://www.geeksforgeeks.org/debugging-in-c/)
- [Depuração e impressão de mensagens de erro em C++](https://www.educative.io/edpresso/debugging-and-error-message-printing-in-cpp)