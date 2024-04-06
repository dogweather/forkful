---
date: 2024-01-20 17:52:07.375478-07:00
description: "Como Fazer: Antigamente, a depura\xE7\xE3o era feita atrav\xE9s de simples\
  \ declara\xE7\xF5es de impress\xE3o. Hoje existem ferramentas de depura\xE7\xE3\
  o dedicadas, mas imprimir\u2026"
lastmod: '2024-04-05T22:51:00.125435-06:00'
model: gpt-4-1106-preview
summary: "Antigamente, a depura\xE7\xE3o era feita atrav\xE9s de simples declara\xE7\
  \xF5es de impress\xE3o."
title: "Exibindo sa\xEDdas de depura\xE7\xE3o"
weight: 33
---

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
