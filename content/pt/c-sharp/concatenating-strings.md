---
title:                "C#: Unindo cadeias de caracteres"
programming_language: "C#"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c-sharp/concatenating-strings.md"
---

{{< edit_this_page >}}

## Por que Concatenar Strings é Importante?

A concatenação de strings é uma técnica comumente usada em programação, que permite a combinação de várias strings em uma única string. Isso é útil para criar mensagens personalizadas, criar arquivos de texto formatados ou simplesmente para facilitar a manipulação de dados. Neste artigo, vamos explorar como podemos utilizar essa técnica na linguagem de programação C#, e como ela pode melhorar a funcionalidade de nossos programas.

## Como Fazer a Concatenação de Strings em C#

Para começar, vamos revisar a sintaxe básica para a concatenação de strings em C#. Para isso, usaremos o operador "+" para juntar duas ou mais strings. Por exemplo:

```C#
string primeiraString = "Olá";
string segundaString = "mundo";
string terceiraString = primeiraString + " " + segundaString;

Console.WriteLine(terceiraString);

// Output:
// Olá mundo
```

Observe que usamos o sinal de "+" para juntar a primeiraString e a segundaString, e também adicionamos um espaço entre elas para que o resultado final fique mais legível. Também é possível usar o operador "+=" para adicionar mais strings a uma variável existente:

```C#
string frase = "Isso é ";
frase += "uma frase.";
Console.WriteLine(frase);

// Output:
// Isso é uma frase.
```

Outra opção é usar o método String.Format(), que permite que você concatene strings e insira valores em determinadas posições. Por exemplo:

```C#
string nome = "João";
int idade = 30;

string frase = String.Format("{0} tem {1} anos de idade.", nome, idade);
Console.WriteLine(frase);

// Output:
// João tem 30 anos de idade.
```

Há também a opção de usar o método Concat() da classe String, que é especialmente útil quando você precisa concatenar mais de duas strings. Por exemplo:

```C#
string primeiraString = "Este é";
string segundaString = "um exemplo";
string terceiraString = "de concatenação.";

string resultado = String.Concat(primeiraString, " ", segundaString, " ", terceiraString);
Console.WriteLine(resultado);

// Output:
// Este é um exemplo de concatenação.
```

## Mergulho Profundo na Concatenação de Strings

Além das técnicas básicas mostradas acima, também existem algumas nuances a serem consideradas quando se trabalha com a concatenação de strings em C#. Por exemplo, é importante ter em mente que a concatenação de strings é mais eficiente quando usamos o operador "+=" ou o método Concat(), e menos eficiente quando usamos o operador "+". Isso porque o operador "+=" e o método Concat() criam uma nova string apenas quando necessário, enquanto o operador "+" sempre cria uma nova string, até mesmo quando não é necessário.

Outra coisa importante a lembrar é que, se você precisa de um desempenho otimizado, é melhor evitar o uso de métodos de concatenação em laços de repetição. Isso pode resultar no aumento desnecessário de alocação de memória e pode afetar o desempenho do seu programa. Em vez disso, é recomendável usar a classe StringBuilder para construir strings dinamicamente em um laço de repetição.

## Veja Também

- Documentação do C#: https://docs.microsoft.com/pt-br/dotnet/csharp/
- Tutorial de Concatenação de Strings em C#: https://www.tutorialspoint.com/csharp/csharp_string_concat.htm
- Desempenho da Concatenação de Strings: https://docs.microsoft.com/pt-br/dotnet/standard/base-types/string-builder