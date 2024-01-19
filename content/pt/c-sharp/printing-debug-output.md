---
title:                "Imprimindo saída de debug"
html_title:           "C#: Imprimindo saída de debug"
simple_title:         "Imprimindo saída de debug"
programming_language: "C#"
category:             "C#"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c-sharp/printing-debug-output.md"
---

{{< edit_this_page >}}

**## O Que e Por Quê?**

A impressão de saída de depuração é uma técnica de programação utilizada para exibir variáveis e outros dados enquanto um programa está sendo executado, auxiliando na identificação de erros. Os programadores a usam por causa de sua eficiência no rastreamento e resolução de bugs durante o desenvolvimento do software.

**## Como Fazer:**

Aqui está um exemplo simples de como imprimir a saída de depuração em C#:

```C#
using System.Diagnostics;

public class DebugExample 
{
    static void Main() 
    {
        Debug.WriteLine("Depuração iniciada");
        int num = 30;
        Debug.WriteLine("O número é " + num);
        Debug.WriteLineIf(num > 20, "O número é maior que 20.");
    }
}
```

A saída seria:

```
Depuração iniciada
O número é 30
O número é maior que 20.
```

Podemos ver que o 'Debug.WriteLine' imprime a informação na saída de depuração e 'Debug.WriteLineIf' imprime apenas se a condição definida for verdadeira.

**## Mergulho Profundo**

A impressão de saída de depuração surgiu bem no início da programação de computadores, como uma necessidade de entender o que estava ocorrendo com os programas durante sua execução.

Como alternativa, a depuração também pode ser feita através de IDEs modernas, que fornecem ferramentas de depuração embutidas. Porém, a impressão de saída de depuração ainda é usada devido a sua simplicidade.

Em C#, a classe 'Debug' é parte do namespace 'System.Diagnostics' e fornece métodos para imprimir a saída de depuração. Ela é excluída na compilação final quando a constante 'DEBUG' é não definida, não afetando o desempenho do aplicativo.

**## Veja Também**

1. [Conceitos de Depuração em C#](https://docs.microsoft.com/pt-br/visualstudio/debugger/debugging-concepts-in-visual-studio?view=vs-2019)
2. [System.Diagnostics.Debug Class](https://docs.microsoft.com/pt-br/dotnet/api/system.diagnostics.debug?view=net-5.0)
3. [Como: Imprimir a Saída de Depuração](https://docs.microsoft.com/pt-br/dotnet/api/system.diagnostics.debug.writeline?view=net-5.0)