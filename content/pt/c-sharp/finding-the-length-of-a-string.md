---
title:                "C#: Encontrando o comprimento de uma sequência"
simple_title:         "Encontrando o comprimento de uma sequência"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c-sharp/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Por que

Ao escrever um programa em C#, muitas vezes é necessário manipular strings, ou seja, sequências de caracteres. Saber a quantidade de caracteres em uma string pode ser útil em diversas situações, como validação de entradas do usuário ou formatação de dados. Neste post, vamos explorar como encontrar o comprimento de uma string em C#.

## Como Fazer

Existem duas maneiras de encontrar o comprimento de uma string em C#: usando o método `Length` do objeto `string` ou usando o método `Count` da classe `Enumerable`.

Primeiro, vejamos um exemplo usando o método `Length`:

```C#
string nome = "Maria";

int comprimento = nome.Length;

Console.WriteLine($"O comprimento da string é: {comprimento}");

// Output: O comprimento da string é: 5
```

No exemplo acima, declaramos uma variável `string` chamada `nome` e atribuímos o valor "Maria" a ela. Em seguida, usamos o método `Length` para obter o comprimento da string e o armazenamos em uma variável `int` chamada `comprimento`. Por fim, imprimimos o resultado na tela.

Agora, vejamos um exemplo usando o método `Count`:

```C#
string frase = "Hoje é um lindo dia";

int comprimento = Enumerable.Count(frase);

Console.WriteLine($"O comprimento da string é: {comprimento}");

// Output: O comprimento da string é: 21
```

Aqui, declaramos uma variável `string` chamada `frase` e atribuímos o valor "Hoje é um lindo dia" a ela. Usamos o método `Count` da classe `Enumerable` para obter o comprimento da string e o armazenamos em uma variável `int` chamada `comprimento`. Por fim, imprimimos o resultado na tela.

Agora que sabemos como encontrar o comprimento de uma string em C#, vamos nos aprofundar um pouco mais neste assunto.

## Profundidade

Ao usar o método `Length` para encontrar o comprimento de uma string, é importante lembrar que ele conta o número de caracteres, incluindo espaços em branco e símbolos especiais. Isso pode ser útil em alguns casos, mas pode também causar diferenças no resultado esperado. Por exemplo, se uma string contém um caractere especial de escape, ele também será contado no resultado do método `Length`.

Já o método `Count`, da classe `Enumerable`, permite que você especifique um predicado como parâmetro, o que pode ser útil para fazer uma contagem mais precisa. Por exemplo, você pode usar o método `Count` para contar apenas as vogais em uma string, ignorando os espaços em branco e outros caracteres.

Em geral, ambos os métodos são úteis para encontrar o comprimento de uma string em C#, mas é importante entender suas diferenças e escolher o mais adequado para cada situação.

## Veja Também

- Documentação oficial sobre o método `Length` (em inglês): https://docs.microsoft.com/en-us/dotnet/api/system.string.length
- Documentação oficial sobre o método `Count` (em inglês): https://docs.microsoft.com/en-us/dotnet/api/system.linq.enumerable.count
- Tutorial sobre strings em C# (em português): https://docs.microsoft.com/pt-br/dotnet/csharp/programming-guide/strings/