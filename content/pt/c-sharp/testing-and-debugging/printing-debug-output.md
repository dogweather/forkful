---
date: 2024-01-20 17:52:35.339403-07:00
description: "How to: (Como Fazer:) Historicamente, imprimir sa\xEDda de debug \xE9\
  \ uma das formas mais diretas de entender o comportamento de um aplicativo - algo\
  \ primitivo,\u2026"
lastmod: '2024-04-05T22:50:59.843329-06:00'
model: gpt-4-1106-preview
summary: "(Como Fazer:) Historicamente, imprimir sa\xEDda de debug \xE9 uma das formas\
  \ mais diretas de entender o comportamento de um aplicativo - algo primitivo, mas\
  \ eficaz."
title: "Exibindo sa\xEDdas de depura\xE7\xE3o"
weight: 33
---

## How to: (Como Fazer:)
```c#
using System;

class Program {
    static void Main() {
        // Imprime informação básica
        Console.WriteLine("Este é um teste de debug.");

        // Imprime variáveis
        int numero = 42;
        Console.WriteLine("O valor da variável é: " + numero);

        // Debug com condição
        bool condicao = true;
        if (condicao) {
            Console.WriteLine("A condição é verdadeira.");
        }

        // Usando interpolação de string
        string nome = "Mundo";
        Console.WriteLine($"Olá, {nome}!");
    }
}
```
Saída:
```
Este é um teste de debug.
O valor da variável é: 42
A condição é verdadeira.
Olá, Mundo!
```

## Deep Dive (Mergulho Profundo)
Historicamente, imprimir saída de debug é uma das formas mais diretas de entender o comportamento de um aplicativo - algo primitivo, mas eficaz. Antes do advento dos IDEs modernos com depuradores sofisticados, programadores dependiam quase que exclusivamente de `Console.WriteLine()` em C#, `printf` em C, ou seus equivalentes em outras linguagens, para rastrear problemas.

Alternativas incluem o uso de ferramentas de perfil (profilers), depuradores (debuggers), e serviços de log mais complexos com níveis de severidade (ex.: Debug, Info, Warn, Error). A implementação de saída de debug pode variar – desde simples mensagens na console até infraestruturas de log complexas que registram o comportamento do aplicativo em produção.

Em C#, o `System.Diagnostics.Trace` e `System.Diagnostics.Debug` fornecem funcionalidades avançadas similares ao `Console.WriteLine()`, mas com a capacidade de desativar a saída de debug em builds de produção e direcionar a saída para diversos ouvintes (listeners).

## See Also (Veja Também)
- [Documentação do .NET sobre depuração e diagnóstico](https://docs.microsoft.com/pt-br/dotnet/core/diagnostics/)
- [Artigo da Microsoft sobre trace e debug](https://docs.microsoft.com/pt-br/dotnet/api/system.diagnostics.trace?view=netcore-3.1)
- [Stack Overflow: Como usar Console.WriteLine em C#](https://pt.stackoverflow.com/questions/tagged/console.writeline+c%23)
