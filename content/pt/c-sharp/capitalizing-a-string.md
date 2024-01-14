---
title:                "C#: Transformando uma string em maiúsculas"
simple_title:         "Transformando uma string em maiúsculas"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c-sharp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

##Por que

Capitalize é uma função útil em C# que permite que os desenvolvedores transformem uma string em letra maiúscula. Isso pode ser útil para garantir a consistência nos dados em um programa ou para melhorar a legibilidade do texto.

## Como Fazer

Para capitalizar uma string em C#, você pode utilizar a função `ToUpper()` do tipo `String`. Veja um exemplo de código abaixo:

```C#
string texto = "este é um exemplo";
string textoCapitalizado = texto.ToUpper();

Console.WriteLine(textoCapitalizado);
```

A saída deste código seria "ESTE É UM EXEMPLO", com todas as letras em maiúsculo. Você também pode usar a função `ToLower()` para transformar todas as letras em minúsculo.

Outra forma de capitalizar uma string é utilizando o método `ToTitleCase` da classe `TextInfo`, que oferece mais opções de formatação. Veja um exemplo:

```C#
string texto = "exemplo de capitalização";
TextInfo textInfo = CultureInfo.CurrentCulture.TextInfo;
string textoCapitalizado = textInfo.ToTitleCase(texto);

Console.WriteLine(textoCapitalizado);
```

O resultado deste código seria "Exemplo De Capitalização". Como você pode ver, a primeira letra de cada palavra foi transformada em maiúscula.

## Profundando

Ao capitalizar uma string, é importante lembrar que a função `ToUpper()` apenas transforma todas as letras em maiúsculo, mas não altera a string original. Isso significa que se você quiser salvar o resultado, é necessário atribuí-lo a uma nova variável, como nos exemplos acima.

Além disso, a função `ToTitleCase` utiliza as regras do idioma atual para formatar a string. Isso significa que em cada idioma, pode haver diferenças na forma como as strings são capitalizadas. Por exemplo, em português, algumas palavras como "de", "da", "do" não são capitalizadas no meio da frase, enquanto em inglês, elas geralmente são. Por isso, é importante ter atenção ao utilizar essa função com diferentes idiomas.

## Veja Também

- [Documentação oficial de C# para a função `ToUpper()`](https://docs.microsoft.com/pt-br/dotnet/api/system.string.toupper?view=net-5.0)
- [Documentação oficial de C# para o método `ToTitleCase()`](https://docs.microsoft.com/pt-br/dotnet/api/system.globalization.textinfo.totitlecase?view=net-5.0)
- [Exemplos de código para capitalizar strings em C#](https://www.c-sharpcorner.com/UploadFile/af66b7/capitalize-firstletter-of-word-through-c-sharp/)