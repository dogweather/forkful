---
title:                "Concatenando strings"
html_title:           "C#: Concatenando strings"
simple_title:         "Concatenando strings"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c-sharp/concatenating-strings.md"
---

{{< edit_this_page >}}

## O que é e por que fazer isso?

Concatenar strings é simplesmente unir duas ou mais strings em uma única string. Os programadores frequentemente fazem isso quando precisam de uma string contendo informações de diferentes variáveis. Isso economiza tempo e torna o código mais eficiente.

## Como fazer:

Um exemplo simples de como concatenar strings em C# é usando o operador "+".

```C#
string nome = "João";
string sobrenome = "Silva";
string nomeCompleto = nome + " " + sobrenome;
Console.WriteLine(nomeCompleto);
```

Este código resultará em "João Silva" sendo impresso na tela.

Também é possível usar o método "Concat()" da classe "String". Veja o exemplo a seguir:

```C#
string texto1 = "Olá";
string texto2 = "mundo";
string texto3 = "!";
string textoFinal = String.Concat(texto1, " ", texto2, texto3);
Console.WriteLine(textoFinal);
```

Este código imprimirá "Olá mundo!".

## Mergulhe Profundamente:

Antes da versão 6 do C#, a concatenação de strings era feita usando o operador "+", o que resultava em um desempenho lento quando muitas strings eram concatenadas. A partir da versão 6, o compilador do C# otimiza automaticamente a concatenação de strings usando o método "Concat()" e o operador "+=".

Existem também outras maneiras de concatenar strings, como usar o método "StringBuilder" da classe "System.Text" ou usar a interpolação de strings, introduzida na versão 6.

## Veja Também:

- [Método Concat()](https://docs.microsoft.com/pt-br/dotnet/api/system.string.concat?view=netcore-3.1)
- [Operadores de Atribuição Compostos](https://docs.microsoft.com/pt-br/dotnet/csharp/language-reference/operators/compound-assignment-operators)
- [Método StringBuilder](https://docs.microsoft.com/pt-br/dotnet/api/system.text.stringbuilder?view=netcore-3.1)