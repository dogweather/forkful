---
title:    "C#: Convertendo uma string para letras minúsculas"
keywords: ["C#"]
---

{{< edit_this_page >}}

# Por que Converter uma String para Minúsculas em C#
Quando se trata de programação, é importante saber como manipular strings de maneira eficiente. Uma tarefa comum é converter uma string para minúsculas. Isso pode ser útil para comparar strings, evitar erros de digitação e garantir a consistência nos dados.

## Como Fazer
Para converter uma string para minúsculas em C#, usamos o método `ToLower()` da classe `string`. Veja um exemplo de código abaixo:

```C#
string texto = "BOM DIA!";
string textoEmMinusculo = texto.ToLower();

Console.WriteLine(textoEmMinusculo);
```

O código acima irá produzir a saída "bom dia!", com todas as letras em minúsculas. Além disso, há também o método `ToLowerInvariant()`, que segue as regras de conversão de minúsculas invariante culturalmente. Isso pode ser útil ao trabalhar com strings em diferentes idiomas.

## Profundidade Técnica
Ao converter uma string para minúsculas em C#, o método `ToLower()` cria uma nova string com os caracteres minúsculos. Por outro lado, o método `ToLowerInvariant()` não cria uma nova string, mas altera os caracteres da string original com base nas regras de conversão invariantes culturalmente. Portanto, é importante considerar o desempenho e a segurança ao decidir qual método usar.

# Veja Também
- [Documentação do Método ToLower()](https://docs.microsoft.com/pt-br/dotnet/api/system.string.tolower?view=net-5.0)
- [Documentação do Método ToLowerInvariant()](https://docs.microsoft.com/pt-br/dotnet/api/system.string.tolowerinvariant?view=net-5.0)