---
title:    "C#: Imprimindo saída de depuração"
keywords: ["C#"]
---

{{< edit_this_page >}}

## Por que

Muitas vezes, ao escrever código em C#, pode ser necessário imprimir valores específicos durante a execução do programa para entender melhor o que está acontecendo. Esse processo é conhecido como "debugging" (depuração) e é essencial para encontrar e corrigir erros em seu código. A impressão de saída de depuração pode ser uma ferramenta útil para facilitar esse processo.

## Como Fazer

Para imprimir valores de depuração em C#, você pode usar a função `Console.WriteLine()`. Esta função aceita como parâmetro uma string e irá imprimi-la no console durante a execução do programa.

```C#
// Exemplo simples de impressão de depuração
int idade = 25;
Console.WriteLine("A idade é: " + idade); // Saída: A idade é: 25
```

Você também pode usar placeholders para imprimir variáveis em uma string. Isso pode ser útil se você precisar imprimir valores com diferentes tipos de dados.

```C#
// Exemplo de impressão de depuração usando placeholders
string nome = "João";
int idade = 25;
Console.WriteLine("O nome é: {0} e a idade é: {1}", nome, idade); // Saída: O nome é: João e a idade é: 25
```

Outra opção é usar a função `Debug.WriteLine()`, que é específica para saídas de depuração. Esta opção é útil quando você está usando um ambiente de desenvolvimento integrado (IDE) como o Visual Studio. A vantagem de usar esta função é que você pode habilitar ou desabilitar a impressão de saída de depuração, o que pode ser útil quando você está lidando com grandes quantidades de dados.

```C#
// Exemplo de uso da função Debug.WriteLine()
int idade = 25;
Debug.WriteLine("A idade é: " + idade); // Saída: A idade é: 25
```

## Profundidade

Além disso, é possível alterar o nível de detalhamento da saída de depuração, utilizando a função `Debug.Indent()` antes da impressão da saída e `Debug.Unindent()` depois. Isso pode ser útil para organizar a saída e torná-la mais legível.

Além disso, você pode usar a diretiva `#if DEBUG` para condicionar a impressão da saída de depuração apenas quando o programa é executado em modo de depuração.

```
// Exemplo de uso da diretiva #if DEBUG
#if DEBUG
Debug.WriteLine("Este trecho será impresso apenas em modo de depuração.");
#endif
```

## Veja Também

- .NET Debugging for Beginners: https://docs.microsoft.com/pt-br/dotnet/standard/debugging-for-beginners/
- How to Debug C# Code: https://docs.microsoft.com/pt-br/visualstudio/debugger/debugging-c-sharp?view=vs-2019
- Visual Studio Debugging Tips and Tricks: https://docs.microsoft.com/pt-br/visualstudio/debugger/debug-features-in-visual-studio?view=vs-2019