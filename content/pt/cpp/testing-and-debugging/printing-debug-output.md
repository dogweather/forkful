---
title:                "Exibindo saídas de depuração"
aliases:
- /pt/cpp/printing-debug-output/
date:                  2024-01-20T17:52:07.375478-07:00
model:                 gpt-4-1106-preview
simple_title:         "Exibindo saídas de depuração"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/cpp/printing-debug-output.md"
---

{{< edit_this_page >}}

## O Que É & Porquê?
Imprimir saída de depuração é o ato de exibir informações de diagnóstico para acompanhar o fluxo e o estado de um programa enquanto ele roda. Programadores fazem isso para rastrear bugs e garantir que tudo esteja funcionando como esperado.

## Como Fazer:
```C++
#include <iostream>
int main() {
    // Código simples para depuração
    int resultado = 42;
    std::cout << "Resultado: " << resultado << std::endl; // Imprime o resultado
}
```
Saída de exemplo:
```
Resultado: 42
```

```C++
#include <iostream>
#include <vector>
int main() {
    std::vector<int> numeros = {1, 2, 3, 4, 5};
    for (int num : numeros) {
        std::cout << num << " "; // Imprime cada elemento do vetor
    }
    std::cout << std::endl; // Quebra de linha no fim
}
```
Saída de exemplo:
```
1 2 3 4 5
```

## Mergulho Profundo:
Antigamente, a depuração era feita através de simples declarações de impressão. Hoje existem ferramentas de depuração dedicadas, mas imprimir saídas ainda é útil, especialmente para localizar problemas rapidamente. 

Uma alternativa é o uso da biblioteca `<cassert>`, que permite afirmar suposições no código, interrompendo o programa quando algo inesperado ocorre.

Em relação à implementação, a saída padronizada do C++, `std::cout`, é sincronizada com o buffer de saída, o que pode causar lentidão. Para depuração de alto desempenho, você pode utilizar `std::cerr`, que é menos custoso pois não é sincronizado.

## Veja Também:
- Documentação do `cin` e `cout`: http://www.cplusplus.com/reference/iostream/
- Ferramentas de depuração do Visual Studio: https://docs.microsoft.com/pt-br/visualstudio/debugger/
- Documentação do gdb (GNU Debugger): https://www.gnu.org/software/gdb/documentation/
- Tutorial da biblioteca `<cassert>`: http://www.cplusplus.com/reference/cassert/
