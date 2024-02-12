---
title:                "Usando um depurador"
aliases:
- /pt/c-sharp/using-a-debugger/
date:                  2024-01-26T03:48:31.961726-07:00
model:                 gpt-4-0125-preview
simple_title:         "Usando um depurador"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c-sharp/using-a-debugger.md"
---

{{< edit_this_page >}}

## O Que & Por Quê?
Usar um depurador significa acessar ferramentas especializadas para testar e diagnosticar código. Programadores fazem isso para eliminar bugs, entender o fluxo do código e garantir que seu código se comporta conforme esperado — é como ter um microscópio para o cérebro do seu código.

## Como fazer:
Imagine que você tem um programinha que não está agindo corretamente:

```C#
static void Main()
{
    int result = Sum(1, 2);
    Console.WriteLine(result);
}

static int Sum(int a, int b)
{
    return a + a; // Oops, deveria ser a + b
}
```

Usando o depurador do Visual Studio, configure um ponto de interrupção clicando na margem esquerda ao lado de `return a + a;`. Quando você executar o programa (com F5), a execução irá pausar lá. Passe o mouse sobre as variáveis para inspecionar seus valores ou use a Janela Imediata para avaliar expressões. Você verá que `a` é 1 e `b` é 2, mas `a + a` não é a soma esperada. Mude para `a + b`, continue executando (F5) e voilà, o console exibe 3.

## Aprofundamento
A história da depuração remonta à década de 1940, quando um bug real (uma traça) foi encontrado em um computador pioneiro. Os depuradores de hoje, como o do Visual Studio, oferecem um conjunto de recursos poderosos, incluindo pontos de interrupção, execução passo a passo, janelas de inspeção e mais.

Alternativas ao depurador do Visual Studio incluem opções de código aberto como GDB para linguagens estilo C ou pdb para Python, e IDEs multiplataforma como JetBrains Rider ou VS Code que oferecem ferramentas de depuração para C# e outras linguagens.

Quando você mergulha na implementação de um depurador, está olhando para um programa que se anexa ao processo da sua aplicação. Ele interpreta o código de máquina, gerencia o estado da memória e controla o fluxo de execução. Isso é coisa pesada que é crucial para uma depuração eficaz, é por isso que o modo de depuração geralmente é mais lento do que o modo de lançamento, onde esses ganchos não existem.

## Veja Também
- [Documentação do Depurador do Visual Studio](https://docs.microsoft.com/pt-br/visualstudio/debugger/)
- [Estratégias de Depuração](https://www.codeproject.com/Articles/79508/Effective-Exception-Handling-in-Visual-C)
