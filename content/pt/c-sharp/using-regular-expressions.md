---
title:                "Utilizando expressões regulares"
html_title:           "C#: Utilizando expressões regulares"
simple_title:         "Utilizando expressões regulares"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c-sharp/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Por que usar expressões regulares em C#?

As expressões regulares são uma ferramenta poderosa e versátil para buscar e manipular padrões de texto em C#. Elas são especialmente úteis para encontrar e validar informações específicas em grandes quantidades de texto, tornando a programação mais eficiente e precisa.

## Como usar expressões regulares em C#

Para utilizar expressões regulares em C#, é necessário importar a biblioteca System.Text.RegularExpressions. A seguir, segue um exemplo de código para buscar o número de telefone em um texto:

```C#
using System.Text.RegularExpressions;

// Texto de exemplo a ser buscado
string texto = "Meu número de telefone é (555) 123-4567.";

// Expressão regular para buscar números de telefone
string regex = @"\(\d{3}\) \d{3}-\d{4}";

// Realizando a busca no texto e imprimindo o resultado
Match resultado = Regex.Match(texto, regex);
Console.WriteLine(resultado.Value); // Output: (555) 123-4567
```

Neste exemplo, a expressão regular utilizada utiliza metacaracteres para encontrar um número de telefone no formato (XXX) XXX-XXXX. A seguir são listados alguns dos metacaracteres mais utilizados em expressões regulares em C#:

- `\d` para encontrar dígitos numéricos;
- `\w` para encontrar caracteres alfanuméricos;
- `\s` para encontrar espaços;
- `.` para encontrar qualquer caractere;
- `+` para encontrar uma ou mais ocorrências do caractere ou grupo anterior;
- `*` para encontrar zero ou mais ocorrências do caractere ou grupo anterior;
- `?` para encontrar zero ou uma ocorrência do caractere ou grupo anterior.

Existem muitos outros metacaracteres e opções que podem ser utilizados em expressões regulares em C#. É recomendado pesquisar e experimentar para entender melhor como eles funcionam.

## Uma visão mais aprofundada sobre expressões regulares em C#

As expressões regulares em C# também permitem o uso de grupos, que permitem agrupar partes da expressão e obter cada grupo separadamente. No exemplo anterior, podemos utilizar grupos para separar o código de área, número e prefixo do número de telefone.

Para utilizar grupos, basta adicionar parênteses na expressão regular. Cada par de parênteses representa um grupo separado. Os grupos podem então ser acessados através da propriedade `Groups` do objeto `Match`.

A seguir, segue um exemplo utilizando grupos para separar o código de área, número e prefixo do número de telefone encontrado anteriormente:

```C#
using System.Text.RegularExpressions;

// Texto de exemplo a ser buscado
string texto = "Meu número de telefone é (555) 123-4567.";

// Expressão regular com grupos para busca de número de telefone
string regex = @"\((\d{3})\) (\d{3})-(\d{4})";

// Realizando a busca no texto
Match resultado = Regex.Match(texto, regex);

// Imprimindo os grupos encontrados
Console.WriteLine(resultado.Groups[0]); // Output: (555) 123-4567
Console.WriteLine(resultado.Groups[1]); // Output: 555
Console.WriteLine(resultado.Groups[2]); // Output: 123
Console.WriteLine(resultado.Groups[3]); // Output: 4567
```

Além disso, vale ressaltar que as expressões regulares em C# também possuem diversos métodos e propriedades úteis para trabalhar com padrões de texto, como `IsMatch()` para verificar se um texto contém um padrão, `Matches()` para retornar todas as ocorrências de um padrão em um texto, entre outros.

## Veja também

- [Documentação da classe Regex em C#](https://docs.microsoft.com/pt-br/dotnet/api/system.text.regularexpressions.regex?view=net-5.0)
- [Expressões regulares em C# - Guia completo (em inglês)](https://www.regular-expressions.info/dotnet.html)
- [Tutorial de expressões regulares em C# (em português)](https://www.devmedia.com.br/expressoes-regulares-como-utilizar-essa-ferramenta-de-busca-em-java/26704)