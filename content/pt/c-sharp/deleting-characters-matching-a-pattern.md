---
title:                "Excluindo caracteres que correspondem a um padrão"
html_title:           "C#: Excluindo caracteres que correspondem a um padrão"
simple_title:         "Excluindo caracteres que correspondem a um padrão"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c-sharp/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Por que

Existem várias razões pelas quais alguém pode precisar deletar caracteres que correspondem a um determinado padrão em um código C#. Alguns exemplos incluem limpar dados de entrada não desejada, formatar strings de acordo com um layout específico ou tornar o código mais legível e organizado.

## Como fazer

Existem várias maneiras de deletar caracteres de acordo com um padrão específico em C#. Aqui estão alguns exemplos de código que você pode usar:

```C#
// Deletar todas as letras minúsculas em uma string
string texto = "Olá mundo!";
string resultado = Regex.Replace(texto, "[a-z]", "");

// Output: "O !"

// Deletar todos os números em uma string
string texto = "123 abc 456 def";
string resultado = Regex.Replace(texto, "[0-9]", "");

// Output: " abc  def"
```

## Mergulho Profundo

A função `Regex.Replace()` é parte da biblioteca de expressões regulares do C# e pode ser usada para substituir caracteres que correspondem a um determinado padrão em uma string. O primeiro parâmetro da função é a string original, o segundo é o padrão a ser correspondido e o terceiro é o texto a ser substituído no lugar dos caracteres excluídos. Além disso, você também pode usar a classe `Regex.Match()` para verificar se uma string contém o padrão desejado antes de fazer a substituição.

## Veja também

- Documentação oficial da biblioteca de expressões regulares do C#: https://docs.microsoft.com/en-us/dotnet/standard/base-types/regular-expression-language-quick-reference
- Tutoriais sobre expressões regulares em C#: https://www.regular-expressions.info/dotnet.html
- Exemplos avançados de uso de expressões regulares em C#: https://www.codeproject.com/Articles/9099/The-30-Minute-Regex-Tutorial