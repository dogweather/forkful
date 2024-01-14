---
title:                "C#: Extraindo subtrings"
programming_language: "C#"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c-sharp/extracting-substrings.md"
---

{{< edit_this_page >}}

## Por que extrair substrings é importante na programação?

Extrair substrings é uma habilidade importante para quem trabalha com programação. Isso permite que o desenvolvedor manipule e utilize partes específicas de uma string em seu código. Isso pode ser útil em diversas situações, como na validação de inputs de usuários, na formatação de dados ou na realização de buscas em textos extensos.

## Como extrair substrings em C#?

Extrair substrings em C# é bastante simples. Para isso, utilizamos o método `Substring()` junto com a classe `String`. Vamos ver um exemplo prático:

```
string texto = "Olá, mundo!";
string substring = texto.Substring(4, 5);
Console.WriteLine(substring);
```

Nesse caso, estamos criando uma variável `texto` que contém a string "Olá, mundo!", e utilizando o método `Substring()` para extrair uma substring começando na quarta letra e com um total de cinco caracteres. Isso resultará na impressão de "mundo" no console.

É importante notar que o primeiro argumento do método `Substring()` é o índice de início da substring, enquanto o segundo argumento é a quantidade de caracteres que a substring deve conter.

## Aprofundando em extrair substrings

Além do exemplo acima, o método `Substring()` também pode ser utilizado com apenas um argumento, como por exemplo `texto.Substring(7)`, que retornaria "mundo!" como resultado. Além disso, é possível utilizar valores negativos para indicar que o início da substring deve ser computado a partir do final da string. Por exemplo, `texto.Substring(-6, 3)` retornaria "mundo!" como resultado.

É importante lembrar que os índices em C# começam a partir do número 0, então para extrair a primeira letra de uma string, utilizamos o índice 0.

## Veja também

- [Documentação oficial do método Substring() em C#](https://docs.microsoft.com/pt-br/dotnet/api/system.string.substring)
- [Tutorial sobre Substrings em C#](https://www.guru99.com/c-sharp-string.html#10)