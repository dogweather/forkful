---
date: 2024-01-26 01:17:14.841084-07:00
description: "Refatora\xE7\xE3o \xE9 o processo de reestrutura\xE7\xE3o de c\xF3digo\
  \ de computador existente sem alterar seu comportamento externo. Programadores fazem\
  \ isso para limpar o\u2026"
lastmod: '2024-03-13T22:44:46.592465-06:00'
model: gpt-4-0125-preview
summary: "Refatora\xE7\xE3o \xE9 o processo de reestrutura\xE7\xE3o de c\xF3digo de\
  \ computador existente sem alterar seu comportamento externo."
title: "Refatora\xE7\xE3o"
weight: 19
---

## O Que & Porquê?

Refatoração é o processo de reestruturação de código de computador existente sem alterar seu comportamento externo. Programadores fazem isso para limpar o código, melhorar a legibilidade, reduzir a complexidade e aumentar a manutenibilidade.

## Como fazer:

Vamos refatorar um simples método C# que calcula e imprime a soma de um array de números:

Antes da Refatoração:
```C#
public class Calculator
{
    public void CalculateSum()
    {
        int[] numbers = { 1, 2, 3, 4, 5 };
        int sum = 0;
        for (int i = 0; i < numbers.Length; i++)
        {
            sum += numbers[i];
        }
        Console.WriteLine("The sum is " + sum);
    }
}
```

Após a Refatoração:
```C#
public class Calculator
{
    private readonly int[] _numbers;

    public Calculator(int[] numbers)
    {
        _numbers = numbers;
    }

    public int CalculateSum()
    {
        return _numbers.Sum();
    }

    public void DisplaySum()
    {
        Console.WriteLine($"The sum is {CalculateSum()}");
    }
}

// Uso:
var calculator = new Calculator(new[] { 1, 2, 3, 4, 5 });
calculator.DisplaySum();
```

Ao refatorarmos, separamos as preocupações, tornamos a classe `Calculator` mais flexível ao permitir que ela receba qualquer array de números, e aproveitamos o LINQ para tornar o cálculo da soma mais conciso.

## Aprofundamento

A refatoração tem suas raízes na comunidade de programação Smalltalk e foi popularizada na década de 1990 pelo livro de Martin Fowler "Refatoração: Melhorando o Design do Código Existente". Ao longo dos anos, tornou-se uma parte fundamental das metodologias ágeis e das boas práticas de codificação.

Há várias abordagens para a refatoração, como o Red-Green-Refactor no Desenvolvimento Guiado por Testes (TDD, na sigla em inglês). Isso garante que a refatoração não introduza bugs, começando com um teste falho, fazendo-o passar e então limpando o código.

Ao implementar a refatoração, é crucial ter uma suíte de testes abrangente para garantir que nenhuma funcionalidade seja quebrada durante o processo. Ferramentas de refatoração automatizadas, como o ReSharper para C#, também podem auxiliar neste processo ao fornecer maneiras seguras de alterar estruturas de código. No entanto, as ferramentas devem ser complementares a um profundo entendimento da base de código e dos princípios de codificação.

## Veja também

- A obra seminal de Martin Fowler sobre Refatoração: [Refatoração: Melhorando o Design do Código Existente](https://martinfowler.com/books/refactoring.html)
- Guia da Microsoft sobre Refatoração no Visual Studio: [Refatoração (C#)](https://docs.microsoft.com/en-us/visualstudio/ide/refactoring-in-visual-studio?view=vs-2022)
- Um olhar detalhado sobre padrões de Refatoração com exemplos: [SourceMaking Refatoração](https://sourcemaking.com/refactoring)
