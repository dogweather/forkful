---
title:                "C#: Concatenando strings"
simple_title:         "Concatenando strings"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c-sharp/concatenating-strings.md"
---

{{< edit_this_page >}}

## Por que usar concatenação de strings?

A concatenação de strings é uma técnica muito comum em programação, especialmente em linguagens orientadas a objetos como C#. Ela permite a combinação de diferentes strings, ou cadeias de caracteres, em uma única string, facilitando a manipulação e exibição de dados em um programa.

## Como fazer

Em C#, a concatenação de strings é realizada utilizando o operador de adição (+) ou o método `Concat` da classe `String`. Vejamos alguns exemplos:

```C#
// Utilizando o operador de adição
string nome = "Pedro";
string sobrenome = "Silva";
string nomeCompleto = nome + " " + sobrenome;
Console.WriteLine(nomeCompleto); // Saída: "Pedro Silva"

// Utilizando o método Concat
string frase = "O " + "rato " + "roeu " + "a " + "roupa " + "do " + "rei";
Console.WriteLine(frase); // Saída: "O rato roeu a roupa do rei"
```

No primeiro exemplo, criamos três strings separadamente e depois as concatenamos utilizando o operador de adição. No segundo exemplo, realizamos a concatenação diretamente no momento de criar a string.

Também é possível utilizar o método `Concat` para concatenar mais de duas strings ao mesmo tempo:

```C#
string frase = String.Concat("O ", "rato ", "roeu ", "a ", "roupa ", "do ", "rei");
Console.WriteLine(frase); // Saída: "O rato roeu a roupa do rei"
```

Note que a concatenação não altera as strings originais, ela simplesmente cria uma nova string combinando as existentes.

## Mergulho Profundo

A concatenação de strings pode ser realizada não apenas com strings fixas, mas também com variáveis e valores de diferentes tipos. No entanto, é importante lembrar que, ao concatenar valores numéricos, eles serão tratados como strings e não como números. Por exemplo:

```C#
int numero = 5;
string texto = "O valor do número é: " + numero; // texto = "O valor do número é: 5"
```

Outra possibilidade é utilizar o método `Join` da classe `String`, que permite concatenar uma lista de strings utilizando um separador. Por exemplo:

```C#
string[] nomes = { "Maria", "João", "Pedro" };
string listaNomes = String.Join(", ", nomes);
Console.WriteLine("Os nomes são: " + listaNomes); // Saída: "Os nomes são: Maria, João, Pedro"
```

## Veja Também

- [Documentação oficial sobre Concatenação de Strings em C#](https://docs.microsoft.com/pt-br/dotnet/csharp/programming-guide/xmldoc/combining-strings)
- [C# Concatenate Strings - concat and join methods](https://www.tutorialsteacher.com/csharp/csharp-string-concat)