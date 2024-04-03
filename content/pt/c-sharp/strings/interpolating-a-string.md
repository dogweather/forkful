---
changelog:
- 2024-02-25, gpt-4-0125-preview, translated from English
date: 2024-02-25 17:06:50.981967-07:00
description: "A interpola\xE7\xE3o de strings em C# permite criar uma nova string\
  \ incluindo express\xF5es dentro de um literal de string, facilitando a formata\xE7\
  \xE3o e a\u2026"
lastmod: '2024-03-13T22:44:46.570454-06:00'
model: gpt-4-0125-preview
summary: "A interpola\xE7\xE3o de strings em C# permite criar uma nova string incluindo\
  \ express\xF5es dentro de um literal de string, facilitando a formata\xE7\xE3o e\
  \ a concatena\xE7\xE3o de strings."
title: Interpolando uma String
weight: 8
---

## Como fazer:
Em C#, a interpolação de string é indicada pelo sinal de dólar (`$`) seguido de um literal de string. Os nomes das variáveis ou expressões são delimitados por chaves (`{}`).

```csharp
string nome = "Jane";
int idade = 28;
string stringInterpolada = $"Olá, {nome}! Você tem {idade} anos.";
Console.WriteLine(stringInterpolada);
// Saída: Olá, Jane! Você tem 28 anos.
```

Em um exemplo mais complexo, você pode realizar operações ou chamar métodos dentro das chaves:

```csharp
double preco = 19.99;
int quantidade = 3;
string detalheDoPedido = $"Preço total: {preco * quantidade:C2}";
Console.WriteLine(detalheDoPedido);
// Saída: Preço total: $59.97
```
O especificador de formato `:C2` dentro das chaves formata o número como uma moeda com duas casas decimais.

Para cenários que requerem formatação mais avançada ou localização, você pode considerar usar o método `string.Format` ou bibliotecas como o Humanizer. O Humanizer pode manipular e exibir strings, datas, horários, intervalos de tempo, números e quantias de maneira mais legível para humanos. Abaixo está um exemplo de uso do Humanizer para manipulação complexa de strings. Note que o Humanizer não faz parte da biblioteca padrão do .NET e requer a instalação do pacote NuGet `Humanizer`.

Primeiro, instale o Humanizer via NuGet:

```
Install-Package Humanizer
```

Em seguida, você pode usá-lo da seguinte forma:

```csharp
using Humanizer;

int diferencaDeDias = 5;
string humanizado = $"O evento foi há {diferencaDeDias} dias.".Humanize();
Console.WriteLine(humanizado);
// Dependendo da configuração e cultura, uma possível saída: O evento foi há 5 dias atrás.
```

Este exemplo demonstra o uso básico. O Humanizer suporta uma ampla gama de funcionalidades que podem ser aplicadas a strings, datas, números e muito mais, tornando suas aplicações mais acessíveis e intuitivas.
