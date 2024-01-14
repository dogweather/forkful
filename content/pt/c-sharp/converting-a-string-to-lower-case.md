---
title:    "C#: Convertendo uma string para minúsculas"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/c-sharp/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Por que converter uma string para letras minúsculas?

Muitas vezes, ao lidar com dados de entrada, é necessário padronizar as strings para garantir que elas sejam comparáveis ​​e fáceis de manipular. Ao converter uma string para letras minúsculas, podemos garantir que todas as letras serão iguais, independentemente de como foram inseridas.

## Como fazer:

Para converter uma string para letras minúsculas em C#, podemos usar o método `ToLower()` da classe `String`. Aqui está um exemplo de código para mostrar como isso pode ser usado:

```C#
string texto = "Exemplo De STRING Caixa Alta";
string textoMin = texto.ToLower();
Console.WriteLine(textoMin);
```

A saída desse código seria "exemplo de string caixa alta". Podemos ver que todas as letras foram transformadas em minúsculas.

## Aprofundando:

Além do método `ToLower()`, existem outras formas de converter uma string para letras minúsculas em C#. Por exemplo, podemos usar o método `ToLowerInvariant()` para garantir que a conversão seja consistente, independentemente da configuração regional do computador.

Outra opção é usar a classe `TextInfo`, que permite especificar uma cultura para a conversão. Podemos usar o método `ToLower()` desta classe para converter a string com base na cultura escolhida.

## Veja também:

- [Guia de Referência C# da Microsoft - Método ToLower()](https://docs.microsoft.com/en-us/dotnet/api/system.string.tolower?view=netcore-3.1)

- [Microsoft Docs - Classe TextInfo](https://docs.microsoft.com/en-us/dotnet/api/system.globalization.textinfo?view=netcore-3.1)

- [Como converter uma string para minúscula em C#](https://www.tutlane.com/tutorial/csharp/csharp-string-lowercase-to-lowerlowerinvariant-textinfo)

Se você está buscando outras formas de manipular strings em C#, confira esses links para mais informações e opções. Esperamos que este artigo tenha sido útil para você!