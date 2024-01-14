---
title:    "C#: Transformando uma string em maiúsculas"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/c-sharp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Por que capitalizar uma string?

Capitalizar uma string pode ser útil em diversas situações de programação, como por exemplo, ao exibir um título ou nome no formato padrão de capitalização. Além disso, quando lidamos com entradas de usuário, é importante ter consistência na maneira como a string é exibida. Por isso, aprender a capitalizar uma string é um conhecimento valioso para qualquer programador.

## Como fazer

Para capitalizar uma string em C#, usamos o método `ToUpper()` da classe `string`. Por exemplo, se tivermos a seguinte string chamada `nome`:

```C#
string nome = "joão";
```

Podemos usá-lo da seguinte forma para capitalizar a primeira letra:

```C#
string nomeCapitalizado = nome.ToUpper();

Console.WriteLine(nomeCapitalizado); // Saída: João
```

Observe que isso apenas capitaliza a primeira letra da string. Se quisermos capitalizar todas as letras, podemos usar o método `ToTitleCase()` da classe `TextInfo`, que é acessível através do objeto `CultureInfo`. Veja o exemplo:

```C#
CultureInfo cultura = new CultureInfo("pt-BR");
TextInfo textInfo = cultura.TextInfo;

string nome = "joão da silva";
string nomeCapitalizadoComMaisculas = textInfo.ToTitleCase(nome);

Console.WriteLine(nomeCapitalizadoComMaisculas); // Saída: João Da Silva
```

## Informações adicionais

A capitalização de strings é um conceito simples, mas é importante ter em mente que esse processo pode ser influenciado pelo contexto cultural. Em idiomas como o português, existem regras específicas para a capitalização de sobrenomes, por exemplo. Portanto, é sempre bom consultar a documentação da linguagem e estar atento a essas diferenças.

## Veja também

- [Documentação oficial da Microsoft sobre o método ToUpper()](https://docs.microsoft.com/en-us/dotnet/api/system.string.toupper?view=net-5.0)
- [Documentação oficial da Microsoft sobre o objeto CultureInfo](https://docs.microsoft.com/en-us/dotnet/api/system.globalization.cultureinfo?view=net-5.0)
- [Stack Overflow: Capitalize first letter of each word in a string using LINQ](https://stackoverflow.com/questions/2730191/capitalize-first-letter-of-each-word-in-a-string-using-linq)