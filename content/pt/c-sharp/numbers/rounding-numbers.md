---
title:                "Arredondamento de números"
aliases:
- /pt/c-sharp/rounding-numbers/
date:                  2024-01-26T03:43:40.737426-07:00
model:                 gpt-4-0125-preview
simple_title:         "Arredondamento de números"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c-sharp/rounding-numbers.md"
---

{{< edit_this_page >}}

## O Que & Porquê?
Arredondar números significa ajustá-los ao valor de lugar mais próximo—pense em simplificá-los. Programadores arredondam para controlar a precisão, aumentar o desempenho ou ao apresentar resultados amigáveis ao usuário—como preços que não precisam de três casas decimais.

## Como fazer:
Aqui está o bilhete de ida e volta para arredondar números em C#:

```csharp
using System;

public class ExemplosDeArredondamento
{
    public static void Main()
    {
        double numeroOriginal = 123.4567;

        // Arredondar para o número inteiro mais próximo
        double arredondado = Math.Round(numeroOriginal);
        Console.WriteLine(arredondado); // Saída: 123

        // Especificar número de casas decimais
        double arredondadoDuasCasasDecimais = Math.Round(numeroOriginal, 2);
        Console.WriteLine(arredondadoDuasCasasDecimais); // Saída: 123.46

        // Arredondar para cima independentemente do próximo dígito
        double arredondadoParaCima = Math.Ceiling(numeroOriginal);
        Console.WriteLine(arredondadoParaCima); // Saída: 124

        // Arredondar para baixo independentemente do próximo dígito
        double arredondadoParaBaixo = Math.Floor(numeroOriginal);
        Console.WriteLine(arredondadoParaBaixo); // Saída: 123
    }
}
```

## Aprofundamento
No passado, arredondar era algo simples para cortar custos computacionais. Cada ciclo contava, e trimar números salvava tempo precioso. Avançando para o moderno C#, trata-se de gerenciar a famosa predisposição de doubles e decimais para erros de precisão e peculiaridades de exibição.

Além do `Math.Round`, `Math.Floor` e `Math.Ceiling`, o enum `MidpointRounding` nos permite ditar o destino de dígitos que estão na corda bamba—é o cruzamento entre as regras bancárias e a justiça do parquinho de "arredondar para cima pela metade".

Para públicos mais exigentes, como aplicações sérias de matemática ou finanças, temos `decimal` em vez de `double`, reduzindo o drama do arredondamento ao oferecer maior precisão—menos arredondamentos, menos problemas.

## Veja Também
- [Documentação Oficial do C# sobre `Math.Round`](https://docs.microsoft.com/en-us/dotnet/api/system.math.round)
- [Stack Overflow: Quando devo usar Double em vez de Decimal?](https://stackoverflow.com/questions/1165761/decimal-vs-double-which-one-should-i-use-and-when)
