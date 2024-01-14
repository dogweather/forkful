---
title:                "C#: Substituindo e substituindo texto."
programming_language: "C#"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c-sharp/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Por que

A busca e substituição de texto é uma tarefa comum em programação. Ela permite que você encontre e substitua determinadas palavras ou padrões de caracteres em um texto ou código, economizando tempo e esforço em alterações repetitivas. Neste artigo, vamos mergulhar em como realizar essa tarefa em C#, uma linguagem de programação popular e versátil.

## Como Fazer

Para realizar a busca e substituição de texto em C#, você pode usar o método `Replace()` da classe `String`. Veja um exemplo abaixo:

```C#
string texto = "Este é um exemplo de texto.";
string novoTexto = texto.Replace("um", "outro");
Console.WriteLine(novoTexto);
```

A saída desse código será "Este é outro exemplo de texto". Como você pode ver, o método `Replace()` substitui todas as ocorrências da palavra "um" na string original pelo valor especificado, "outro" neste caso.

Você também pode usar expressões regulares para realizar busca e substituição de texto em C#. As expressões regulares são padrões de caracteres que permitem buscar por padrões específicos em um texto. Veja um exemplo abaixo:

```C#
using System.Text.RegularExpressions;

string texto = "5 bananas, 2 maçãs, 3 laranjas";
string novoTexto = Regex.Replace(texto, @"\d+", "uma");
Console.WriteLine(novoTexto);
```

A saída desse código será "uma bananas, uma maçãs, uma laranjas". Aqui, usamos a expressão regular `\d+` para buscar por qualquer sequência de dígitos na string original e substituí-los por "uma".

## Mergulho Profundo

Além do método `Replace()` e expressões regulares, existem outras formas de realizar busca e substituição de texto em C#. Você pode usar bibliotecas externas, como a `StringSearch` ou `RegexReplace`, que oferecem recursos mais avançados para manipulação de strings.

Também é importante mencionar que o uso de expressões regulares pode ser mais complexo e, às vezes, menos eficiente do que outras abordagens, como o uso do método `Replace()`. Portanto, é sempre uma boa prática avaliar sua necessidade antes de utilizar expressões regulares.

## Veja Também

- [Documentação oficial do método Replace()](https://docs.microsoft.com/pt-br/dotnet/api/system.string.replace?view=net-5.0)
- [Tutorial sobre expressões regulares em C#](https://www.devmedia.com.br/expressoes-regulares-em-csharp/19163)
- [Biblioteca StringSearch](https://github.com/EamonNerbonne/CharSequencing)
- [Biblioteca RegexReplace](https://github.com/spiegelsoft/RegexReplace)