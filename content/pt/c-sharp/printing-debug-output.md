---
title:                "Imprimindo saída de depuração"
html_title:           "C#: Imprimindo saída de depuração"
simple_title:         "Imprimindo saída de depuração"
programming_language: "C#"
category:             "C#"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c-sharp/printing-debug-output.md"
---

{{< edit_this_page >}}

## O que e por que fazer? 
Debugar é um processo indispensable para todos os programadores. Isso envolve a identificação e correção de erros e bugs em código. Ao imprimir saídas de debug, os programadores têm uma forma fácil e rápida de entender o que está acontecendo no código e identificar possíveis erros.

## Como fazer:
```C#
Console.WriteLine("Mensagem de Debug");
```
Esta é a forma mais comum de imprimir saídas de debug em C#. A mensagem será exibida no terminal quando o código for executado. Outra opção é utilizar a classe `Debug` do namespace `System.Diagnostics` para imprimir informações de debug em locais específicos no código.

## Mergulho Profundo:
O processo de debug existe desde os primórdios da programação. Em seus primeiros dias, os programadores utilizavam papel e caneta para identificar e corrigir erros em seus códigos. Com o avanço da tecnologia, ferramentas específicas foram sendo desenvolvidas para facilitar o processo de debug, como o depurador (debugger) presente nos ambientes de desenvolvimento integrado (IDEs). Além disso, existem outras técnicas de debug, como breakpoint (ponto de interrupção) e visualização de variáveis, para facilitar a identificação de erros.

## Veja Também:
- [Artigo sobre Debugging da Microsoft](https://docs.microsoft.com/pt-br/visualstudio/debugger/debugger-feature-tour?view=vs-2019)
- [Ferramentas de Debug para C#](https://www.lifewire.com/debugging-tools-for-c-4143683)
- [Tutorial de Debugging em C#](https://stackify.com/csharp-debugging-tips/)