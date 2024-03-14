---
date: 2024-01-26 03:48:22.062943-07:00
description: "Usar um depurador significa ativar uma ferramenta que permite espiar\
  \ dentro do seu programa em execu\xE7\xE3o para entender o que realmente est\xE1\
  \ acontecendo.\u2026"
lastmod: '2024-03-13T22:44:46.884054-06:00'
model: gpt-4-0125-preview
summary: "Usar um depurador significa ativar uma ferramenta que permite espiar dentro\
  \ do seu programa em execu\xE7\xE3o para entender o que realmente est\xE1 acontecendo.\u2026"
title: Usando um depurador
---

{{< edit_this_page >}}

## O Que & Por Quê?
Usar um depurador significa ativar uma ferramenta que permite espiar dentro do seu programa em execução para entender o que realmente está acontecendo. Programadores fazem isso para encontrar e eliminar bugs — aqueles problemas irritantes que fazem seu código se comportar de forma inesperada ou travar.

## Como fazer:
C++ se integra com depuradores como o GDB ou o depurador do Visual Studio. Aqui está um pequeno exemplo usando o GDB:

```C++
#include <iostream>

int main() {
    int a = 5;
    int b = 0;
    int c = a / b; // Ops, divisão por zero!
    std::cout << c << std::endl;
    return 0;
}

// Compile com:
// g++ -g -o meu_programa meu_programa.cpp

// Execute com o depurador:
// gdb ./meu_programa
```

Uma vez que você iniciou o GDB, você pode definir pontos de interrupção, acompanhar o código passo a passo, inspecionar variáveis e muito mais. Se executar o exemplo acima, você verá seu programa travar devido à divisão por zero.

## Mergulho Profundo

A depuração tem suas raízes nos primeiros dias da programação, onde remover literalmente bugs (insetos!) do hardware era necessário. Desde então, as ferramentas de depuração evoluíram para softwares complexos e poderosos, críticos para o desenvolvimento.

Alternativas ao GDB para C++ incluem o LLDB, bem como depuradores integrados às IDEs como os do Visual Studio, CLion ou Eclipse. Esses ambientes modernos fornecem interfaces gráficas que tornam a depuração menos intimidadora.

Detalhes de implementação sobre o uso de um depurador muitas vezes dependem do seu ambiente de desenvolvimento:

- Depuradores de linha de comando (GDB, LLDB) requerem familiaridade com comandos de terminal e muitas vezes envolvem uma curva de aprendizado mais íngreme.
- Depuradores gráficos simplificam o processo, permitindo interações de apontar e clicar para definir pontos de interrupção, acompanhar o código passo a passo e observar variáveis.

Entender as capacidades do seu depurador, como pontos de interrupção condicionais, pontos de observação ou avaliação de expressões, pode aumentar significativamente sua eficiência no diagnóstico de problemas.

## Veja Também
- [Documentação do GDB](https://www.gnu.org/software/gdb/documentation/)
- [Documentação de Comandos do LLDB](https://lldb.llvm.org/use/map.html)
- [Tutorial do Depurador do Visual Studio](https://docs.microsoft.com/pt-br/visualstudio/debugger/debugger-feature-tour)
- [Depuração com CLion](https://www.jetbrains.com/help/clion/debugging-code.html)
