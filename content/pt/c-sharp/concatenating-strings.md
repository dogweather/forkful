---
title:    "C#: Concatenando strings"
keywords: ["C#"]
---

{{< edit_this_page >}}

## Por que utilizar a concatenação de strings em C#

Ao escrever um programa ou aplicação em C#, muitas vezes nos deparamos com a necessidade de combinar diferentes textos em uma única string. Isso pode ser útil para criar mensagens personalizadas, gerar relatórios ou formatar dados de maneira mais legível. Nesses casos, a concatenação de strings é a solução perfeita.

## Como fazer a concatenação de strings em C#

Existem várias formas de combinar strings em C#, mas a maneira mais simples é utilizar o operador "+" entre duas ou mais variáveis de texto. Veja o exemplo abaixo:

```C#
string saudacao = "Olá, ";
string nome = "João";
string mensagem = saudacao + nome;
Console.WriteLine(mensagem); // Saída: "Olá, João"
```

Também é possível fazer a concatenação de strings utilizando o método `Concat()` da classe `String`. Dessa forma, podemos combinar mais de duas strings de uma só vez. Confira:

```C#
string primeiroNome = "Maria";
string sobrenome = "Silva";
string apelido = "Mary";
string nomeCompleto = String.Concat(primeiroNome, sobrenome, apelido);
Console.WriteLine(nomeCompleto); // Saída: "MariaSilvaMary"
```

## Uma análise mais profunda sobre a concatenação de strings

Quando utilizamos o operador "+" para concatenar strings, o compilador do C# converte automaticamente o código em uma chamada ao método `Concat()`. Isso significa que, na prática, não há diferença entre as duas formas de concatenação apresentadas no tópico anterior.

É importante ressaltar que, apesar de ser uma operação básica, a concatenação de strings pode afetar o desempenho de seu código, especialmente quando estamos lidando com grandes quantidades de texto. Nesses casos, é recomendado utilizar a classe `StringBuilder` em vez do operador "+". Isso porque, em cada concatenação feita com o operador, o compilador cria uma nova string na memória, o que pode ser bastante custoso.

Com a classe `StringBuilder`, é possível adicionar novos caracteres à mesma string sem precisar alocar memória a todo momento. Dessa forma, o processo de concatenação é mais eficiente e rápido.

## Veja também

- [Documentação oficial do C# sobre concatenação de strings (em inglês)](https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/operators/string-concatenation)
- [Artigo sobre otimização de performance em operações com strings em C# (em português)](https://www.codigofonte.com.br/artigos/performance-de-codigo-em-net-otimizacao-deu-string-vish)