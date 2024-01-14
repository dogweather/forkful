---
title:                "C#: Buscando e substituindo texto"
simple_title:         "Buscando e substituindo texto"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c-sharp/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Por que fazer busca e substituição de texto?

A busca e substituição de texto é uma tarefa comum em programação. Ela permite que os desenvolvedores localizem e alterem pedaços específicos de código de forma eficiente. Além disso, essa técnica também pode ser útil para correção de erros em grandes blocos de texto.

## Como fazer busca e substituição de texto em C#

Há diferentes maneiras de realizar busca e substituição de texto em C#. Uma opção é utilizar o método `Replace()` da classe `String`, que permite que você substitua uma determinada sequência de caracteres por outra em uma string específica. Veja um exemplo:

```
// Código de exemplo em C#
string texto = "Hello, world!";
string novoTexto = texto.Replace("world", "mundo");

Console.WriteLine(novoTexto);

// Output: Hello, mundo!
```

Você também pode realizar busca e substituição utilizando expressões regulares com a classe `Regex`. Este método é útil quando se deseja substituir padrões específicos de texto. Veja o exemplo abaixo:

```
// Código de exemplo em C#
string texto = "Welcome to my blog, my dear readers!";
string novoTexto = Regex.Replace(texto, "my ", "");

Console.WriteLine(novoTexto);

// Output: Welcome to blog, dear readers!
```

## Análise aprofundada de busca e substituição de texto

Além do método `Replace()` e das expressões regulares, o .NET Framework possui outras opções para busca e substituição de texto, como as classes `StringBuilder` e `StringBuilderExtensions`, que oferecem desempenho significativamente superior ao método `Replace()` quando se trata de grandes quantidades de texto.

Outra opção é o método `Replace()` da classe `StringBuffer`, que oferece flexibilidade adicional para manipulação de strings imutáveis.

Independentemente do método escolhido, é importante entender as diferenças de desempenho e funcionalidade entre eles para utilizar a técnica de busca e substituição de texto de forma eficaz.

## Veja também

- [Documentação oficial do método Replace() em C#](https://docs.microsoft.com/pt-br/dotnet/api/system.string.replace)
- [Documentação oficial da classe Regex em C#](https://docs.microsoft.com/pt-br/dotnet/api/system.text.regularexpressions.regex)
- [Documentação oficial da classe StringBuilder em C#](https://docs.microsoft.com/pt-br/dotnet/api/system.text.stringbuilder)
- [Documentação oficial da classe StringBuilderExtensions em C#](https://docs.microsoft.com/pt-br/dotnet/api/system.text.stringbuilderextensions)
- [Documentação oficial do método Replace() da classe StringBuffer em C#](https://docs.microsoft.com/pt-br/dotnet/api/system.text.stringbuffer.replace)