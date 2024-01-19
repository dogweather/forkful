---
title:                "Concatenando strings"
html_title:           "Elixir: Concatenando strings"
simple_title:         "Concatenando strings"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c-sharp/concatenating-strings.md"
---

{{< edit_this_page >}}

# Concatenando Strings em C#

## O Que & Por quê?

Concatenar strings é basicamente a técnica de unir duas ou mais strings em uma única. Nos programadores fazemos isso para manipular ou exibir eficientemente dados baseados em texto.

## Como Fazer:

Vamos dar uma olhada em um exemplo de como concatenar strings em C#.

```C#
string nome = "João";
string sobrenome = "Silva";

// Concatenando usando o operador '+'
string nomeCompleto = nome + " " + sobrenome;
Console.WriteLine(nomeCompleto); // Saída: João Silva
```

```C#
// Concatenando usando a função String.Concat
string nomeCompleto2 = String.Concat(nome, " ", sobrenome);
Console.WriteLine(nomeCompleto2); // Saída: João Silva
```

```C#
// Concatenando usando a função String.Format
string nomeCompleto3 = String.Format("{0} {1}", nome, sobrenome);
Console.WriteLine(nomeCompleto3); // Saída: João Silva
```

```C#
// Concatenando usando a função StringBuilder
StringBuilder sb = new StringBuilder();
sb.Append(nome).Append(" ").Append(sobrenome);
Console.WriteLine(sb.ToString()); // Saída: João Silva
```

## Deep Dive

Ao longo da história, a concatenação de strings em C# evoluiu para oferecer métodos mais eficientes e flexíveis. Por exemplo, o 'StringBuilder' é apropriado para situações em que muitas operações de concatenação estão ocorrendo, porque ele é projetado para ter um melhor desempenho.

Existem alternativas para a concatenação de strings, dependendo da situação. Em C# 6.0 e versões posteriores, temos a interpolação de strings que permite incluir diretamente o valor das variáveis na string.

```C#
// Concatenando as strings usando Interpolation
string nomeCompleto4 = $"{nome} {sobrenome}";
Console.WriteLine(nomeCompleto4); // Saída: João Silva
```

A implementação da concatenação de strings varia de acordo com o método que você escolher, mas em todos os casos, o objetivo é unir strings de maneira eficiente e legível.

## Veja Também

[Aqui](https://docs.microsoft.com/pt-br/dotnet/csharp/programming-guide/strings/) você pode encontrar a documentação oficial da Microsoft sobre strings em C#. E [aqui](https://docs.microsoft.com/pt-br/dotnet/api/system.string?view=net-5.0) você pode explorar mais sobre as funções e métodos disponíveis para manipulação de strings.