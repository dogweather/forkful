---
title:                "C#: Exclusão de caracteres correspondentes a um padrão"
programming_language: "C#"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c-sharp/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Por que

Muitas vezes, ao trabalhar com strings e textos em C#, pode ser necessário remover certos caracteres que correspondam a um determinado padrão. Isso pode ser útil para limpar dados da entrada do usuário ou para formatar o texto de uma maneira específica. Neste artigo, vamos explorar como podemos fazer isso no C#.

## Como Fazer

Existem algumas maneiras de deletar caracteres que correspondam a um padrão em C#. Uma delas é usar o método `string.Replace()` combinado com expressões regulares para especificar o padrão que queremos deletar. Vamos dar uma olhada neste exemplo de código:

```C#
string texto = "Este é um texto de exemplo que contém @ e # caracteres especiais.";
string textoCorrigido = Regex.Replace(texto, "[#@]", "");
Console.WriteLine(textoCorrigido);
```

Aqui, usamos a classe `Regex` para especificar os caracteres `@` e `#` que queremos deletar do texto. O resultado retornado pela chamada do método `Replace()` é uma nova string sem esses caracteres. No exemplo acima, a saída seria: "Este é um texto de exemplo que contém e caracteres especiais."

Além disso, podemos utilizar o método `string.Trim()` para remover espaços em branco ou caracteres indesejados do início ou do fim de uma string. Por exemplo:

```C#
string texto = "   Exemplo de texto com espaços em branco    ";
string textoTrimmed = texto.Trim();
Console.WriteLine(textoTrimmed);
```

O resultado seria a nova string "Exemplo de texto com espaços em branco". Podemos também usar o método `string.TrimStart()` ou `string.TrimEnd()` se quisermos especificar o lado da string que queremos "aparar".

## Deep Dive

Remover caracteres correspondentes a um padrão pode ser útil em diversas situações. Além dos exemplos acima, podemos usar essa técnica para validar entradas do usuário em um formulário ou para formatar corretamente um número de telefone. É importante lembrar que podemos usar expressões regulares para especificar padrões mais complexos, o que torna essa técnica ainda mais poderosa.

## Veja Também

- [Documentação da classe `Regex` em C#](https://docs.microsoft.com/pt-br/dotnet/api/system.text.regularexpressions.regex?view=net-5.0)
- [Guia de expressões regulares em C#](https://csharp-station.com/Tutorial/CSharp/Lesson11)
- [Tutorial de formatação de strings em C#](https://www.tutorialsteacher.com/csharp/csharp-string-format)