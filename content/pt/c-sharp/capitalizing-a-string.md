---
title:                "C#: Convertendo uma cadeia de caracteres em maiúsculas."
programming_language: "C#"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c-sharp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Por que capitalizar uma string?

A capitalização de uma string é um conceito básico e essencial em qualquer linguagem de programação. Isso permite que você transforme uma determinada string em um formato que atenda às suas necessidades, como tornar a primeira letra de cada palavra maiúscula.

## Como capitalizar uma string em C#

Em C#, a capitalização de uma string é realizada utilizando o método ".ToUpper()" ou ".ToLower()". O primeiro converte todas as letras da string em maiúsculas, enquanto o segundo converte tudo para minúsculas.

```
C# código de exemplo:
string texto = "exemplo de string";
Console.WriteLine("String original: " + texto);
Console.WriteLine("String em maiúsculas: " + texto.ToUpper());
Console.WriteLine("String em minúsculas: " + texto.ToLower()); 
```

Saída:

```
String original: exemplo de string
String em maiúsculas: EXEMPLO DE STRING
String em minúsculas: exemplo de string
```

## Profundidade na capitalização de string

Além das funções ".ToUpper()" e ".ToLower()", o C# também oferece a possibilidade de capitalizar apenas a primeira letra de uma string com o método ".ToTitleCase()". Além disso, é possível especificar o idioma para garantir que a capitalização seja feita de acordo com as regras gramaticais daquele idioma.

```
C# código de exemplo:
string texto = "exemplo de string";
Console.WriteLine("String original: " + texto);
Console.WriteLine("String com a primeira letra maiúscula: " + CultureInfo.CurrentCulture.TextInfo.ToTitleCase(texto));
```

Saída:

```
String original: exemplo de string
String com a primeira letra maiúscula: Exemplo De String
```

## Veja também

- [Documentação oficial da Microsoft sobre capitalização de strings em C#] (https://docs.microsoft.com/pt-br/dotnet/standard/base-types/transforming-strings)
- [Tutorial do DevMedia sobre capitalização de strings em C#] (https://www.devmedia.com.br/csharp-tostring-formatando-tipo-strings-e-datas/31972)
- [Exemplo prático de capitalização de strings com C#] (https://www.youtube.com/watch?v=EGng16GVx0g)