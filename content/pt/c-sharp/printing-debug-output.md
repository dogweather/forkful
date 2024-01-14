---
title:    "C#: Imprimindo saída de depuração"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/c-sharp/printing-debug-output.md"
---

{{< edit_this_page >}}

## Por que imprimir saída de depuração é importante

Imprimir saída de depuração ou "debug output" é uma ferramenta fundamental para qualquer programador. Essa técnica permite visualizar informações sobre o comportamento do programa enquanto ele está sendo executado, o que é extremamente útil para detectar e corrigir erros no código. Além disso, também pode ser útil para entender melhor como o programa funciona e identificar possíveis melhorias.

## Como fazer

Para imprimir saída de depuração em C#, você pode usar a função "Console.WriteLine()". Essa função exige um parâmetro entre parênteses, que é a informação que será exibida no console. Por exemplo:

```C#
Console.WriteLine("Aqui está um exemplo de saída de depuração.");
```

Isso imprimirá a frase "Aqui está um exemplo de saída de depuração." no console do terminal. Você também pode usar a função "Console.Write()" para imprimir sem pular uma linha.

Outra opção é usar a classe "Debug" do namespace "System.Diagnostics". Esta classe oferece métodos úteis para imprimir informações de depuração, como "Debug.WriteLine()" e "Debug.Assert()". Você pode encontrar mais informações sobre a classe "Debug" na documentação oficial da Microsoft.

## Mergulho Profundo

Há várias maneiras de personalizar a saída de depuração em C#. Por exemplo, você pode usar o símbolo de escape "\n" para pular uma linha na saída. Além disso, você pode usar a classe "Trace" para criar uma saída de depuração mais detalhada e também usar a classe "Debugger" para adicionar breakpoints ao seu código e parar a execução para visualizar os valores das variáveis.

Outra técnica interessante é usar o recurso de "Conditional Debugging", que permite imprimir a saída de depuração apenas quando uma condição específica é atendida. Isso pode economizar tempo e evitar a impressão desnecessária de informações de depuração.

Em suma, imprimir saída de depuração não é apenas útil, mas também pode ajudar a melhorar a qualidade do seu código e facilitar a identificação e correção de erros.

## Veja também

- [Documentação oficial do C# Debugging](https://docs.microsoft.com/pt-br/dotnet/core/tutorials/debugging-with-visual-studio)
- [Exibindo informações de depuração em C#](https://www.c-sharpcorner.com/UploadFile/011993/how-to-display-debugging-information-in-console-in-C-Sharp/)
- [Usando a classe "Trace" em C#](https://www.tutorialsteacher.com/articles/how-to-use-trace-for-debugging-in-csharp)
- [Usando a classe "Debugger" em C#](https://www.tutorialsteacher.com/articles/how-to-debug-csharp-code-in-visual-studio)