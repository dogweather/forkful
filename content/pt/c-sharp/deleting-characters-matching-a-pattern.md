---
title:    "C#: Excluindo caracteres que correspondem a um padrão"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/c-sharp/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Por que

Às vezes, enquanto estamos escrevendo códigos em C#, pode ser necessário deletar caracteres que correspondam a um padrão específico. Isso pode ser necessário para limpar strings de dados desnecessários ou para realizar operações específicas. Neste artigo, vamos explorar como fazer isso da maneira mais eficiente possível.

## Como fazer

Para deletar caracteres que correspondam a um padrão em C#, podemos usar métodos já existentes na linguagem, como o método `Replace()` da classe `String`. Este método aceita dois parâmetros: o padrão a ser substituído e o novo padrão que substituirá o anterior. Por exemplo:

```C#
string frase = "Olá, meu nome é João";
string novaFrase = frase.Replace("é", "am");

Console.WriteLine(novaFrase);
// Output: Olá, meu nome am João
```

Também podemos usar expressões regulares para encontrar e substituir caracteres correspondentes. Isso pode ser útil quando precisamos de uma abordagem mais poderosa e flexível. Veja um exemplo:

```C#
string texto = "Hoje é dia de festa!";
string novoTexto = Regex.Replace(texto, @"[!]", "");

Console.WriteLine(novoTexto);
// Output: Hoje é dia de festa
```

No exemplo acima, usamos uma expressão regular para substituir todos os caracteres de ponto de exclamação por uma string vazia, efetivamente deletando-os da string original.

## Mergulho Profundo

Além dos métodos mencionados acima, existem outras maneiras de deletar caracteres que correspondam a um padrão em C#. Uma delas é usando o método `Remove()` da classe `String`, que aceita um índice inicial e a quantidade de caracteres a serem removidos. Veja um exemplo:

```C#
string texto = "Essa string tem um espaço em branco.";
string novaString = texto.Remove(10, 6);

Console.WriteLine(novaString);
// Output: Essa stringtem um espaço em branco.
```

Outra maneira é utilizar o método `Substring()` combinado com o método `IndexOf()`. O `IndexOf()` retornará o índice do primeiro caractere do padrão e o `Substring()` retornará uma nova string a partir desse índice. Confira:

```C#
string texto = "String com espaços antes e depois";
string novoTexto = texto.Substring(texto.IndexOf("com"));

Console.WriteLine(novoTexto);
// Output: com espaços antes e depois
```

## Veja também

- [Documentação do método Replace()](https://docs.microsoft.com/pt-br/dotnet/api/system.string.replace?view=netcore-3.1)
- [Documentação da classe Regex](https://docs.microsoft.com/pt-br/dotnet/api/system.text.regularexpressions.regex?view=netcore-3.1)
- [Documentação do método Remove()](https://docs.microsoft.com/pt-br/dotnet/api/system.string.remove?view=netcore-3.1)
- [Documentação do método Substring()](https://docs.microsoft.com/pt-br/dotnet/api/system.string.substring?view=netcore-3.1)