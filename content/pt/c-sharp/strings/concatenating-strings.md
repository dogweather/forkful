---
title:                "Concatenando strings"
date:                  2024-01-20T17:34:38.280980-07:00
model:                 gpt-4-1106-preview
simple_title:         "Concatenando strings"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c-sharp/concatenating-strings.md"
---

{{< edit_this_page >}}

## O Que & Porquê?

Concatenar strings significa juntar duas ou mais sequências de caracteres em uma só. Programadores fazem isso para construir mensagens, caminhos de arquivos, ou qualquer outro dado que precise ser unido ou manipulado como um texto completo.

## Como Fazer:

```C#
string hello = "Olá";
string world = "Mundo";
string message = hello + " " + world + "!"; // Concatenação simples com o operador +

Console.WriteLine(message); // Saída: Olá Mundo!

string[] words = { "Concatenação", "com", "arrays" };
string sentence = String.Join(" ", words); // Utilizando String.Join para concatenar elementos de um array

Console.WriteLine(sentence); // Saída: Concatenação com arrays

StringBuilder builder = new StringBuilder(); // A StringBuilder para concatenações eficientes em loops ou muitas operações
builder.Append("StringBuilder ");
builder.Append("é ");
builder.Append("útil.");
string result = builder.ToString();

Console.WriteLine(result); // Saída: StringBuilder é útil.
```

## Mergulho Profundo:

Na era pré-dotNET Framework 2.0, a concatenação era frequentemente feita com o operador `+`, mas isso podia ser ineficiente devido à imutabilidade das strings em C#. Cada concatenação resultava em um novo objeto string na memória. Para evitar tal desperdício, a classe `StringBuilder` foi concebida, otimizando a performance especialmente em situações com várias concatenações, como em loops.

Alternativas modernas incluem o método `String.Concat`, `String.Join`, e interpolação de strings com o símbolo `$` desde o C# 6, que também melhoram a legibilidade e a eficiência da concatenação.

```C#
// Interpolação de strings
string name = "Mundo";
string greeting = $"Olá, {name}!"; // Com a interpolação de strings temos uma sintaxe limpa e eficiente.

Console.WriteLine(greeting); // Saída: Olá, Mundo!
```

A seguir, as implementações e como elas funcionam:
- `+` cria uma nova string a cada operação.
- `String.Concat` e `String.Join` são otimizados para concatenar de uma vez, sem criar múltiplas strings intermediárias.
- `StringBuilder` mantém um buffer interno que cresce conforme necessário para acomodar a string construída, o que é muito mais eficiente que criar muitas strings temporárias.
- Interpolação usa `String.Format` internamente e também evita criações excessivas de strings particulares.

## Veja Também:

- Documentação da Microsoft sobre strings em C#: https://docs.microsoft.com/pt-br/dotnet/csharp/programming-guide/strings/
- Uma visão geral sobre interpolação de strings: https://docs.microsoft.com/pt-br/dotnet/csharp/language-reference/tokens/interpolated
- `StringBuilder` Classe: https://docs.microsoft.com/pt-br/dotnet/api/system.text.stringbuilder?view=net-6.0
- Comparação de desempenho entre diferentes métodos de concatenação: https://dotnetcoretutorials.com/2020/05/10/string-concatenation-vs-stringbuilder-vs-string-format-vs-interpolated-strings/
