---
title:    "C#: Calculando uma data no futuro ou no passado."
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/c-sharp/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Por que

Calcular datas no futuro ou no passado pode ser uma tarefa importante em muitos projetos de programação. Por exemplo, em aplicações que envolvem agendamentos, é necessário saber quando uma tarefa será realizada ou quando um evento passou ou acontecerá no calendário. Aprender a calcular datas pode ser útil em diversas situações e é uma habilidade importante para programadores.

## Como fazer

A linguagem de programação C# possui diversas funções e métodos que facilitam o cálculo de datas no futuro ou no passado. Vamos ver algumas maneiras de como fazer isso utilizando exemplos de código, utilizando o formato "```C# ... ```".

### Calcular data futura a partir de uma data base

Método 1: Utilizando o método `AddDays()`.

```
C# DateTime dataBase = new DateTime(2021, 10, 19);

// Adicionando 10 dias à data base
DateTime dataFutura = dataBase.AddDays(10);

Console.WriteLine($"Data base: {dataBase.ToString("dd/MM/yyyy")}");
Console.WriteLine($"Data futura: {dataFutura.ToString("dd/MM/yyyy")}");

// Output:
// Data base: 19/10/2021
// Data futura: 29/10/2021
```

Método 2: Utilizando o operador `+`.

```
C# DateTime dataBase = new DateTime(2021, 10, 19);

// Adicionando 10 dias à data base
DateTime dataFutura = dataBase + TimeSpan.FromDays(10);

Console.WriteLine($"Data base: {dataBase.ToString("dd/MM/yyyy")}");
Console.WriteLine($"Data futura: {dataFutura.ToString("dd/MM/yyyy")}");

// Output:
// Data base: 19/10/2021
// Data futura: 29/10/2021
```

### Calcular data passada a partir de uma data base

Método 1: Utilizando o método `AddDays()`.

```
C# DateTime dataBase = new DateTime(2021, 10, 19);

// Subtraindo 10 dias da data base
DateTime dataPassada = dataBase.AddDays(-10);

Console.WriteLine($"Data base: {dataBase.ToString("dd/MM/yyyy")}");
Console.WriteLine($"Data passada: {dataPassada.ToString("dd/MM/yyyy")}");

// Output:
// Data base: 19/10/2021
// Data passada: 09/10/2021
```

Método 2: Utilizando o operador `-`.

```
C# DateTime dataBase = new DateTime(2021, 10, 19);

// Subtraindo 10 dias da data base
DateTime dataPassada = dataBase - TimeSpan.FromDays(10);

Console.WriteLine($"Data base: {dataBase.ToString("dd/MM/yyyy")}");
Console.WriteLine($"Data passada: {dataPassada.ToString("dd/MM/yyyy")}");

// Output:
// Data base: 19/10/2021
// Data passada: 09/10/2021
```

## Aprofundando no assunto

Além dos métodos mostrados acima, existem muitas outras maneiras de calcular datas no futuro ou no passado utilizando C#. É importante também ter conhecimento sobre formatos de data, variáveis de data especiais e como lidar com datas em diferentes fusos horários.

Para se aprofundar, recomendamos ler a documentação oficial do C# sobre datas e também explorar os métodos disponíveis na classe `DateTime`.

## Veja também

- [Documentação oficial do C# sobre datas](https://docs.microsoft.com/pt-br/dotnet/api/system.datetime?view=net-5.0)
- [Lista de métodos disponíveis na classe `DateTime`](https://docs.microsoft.com/pt-br/dotnet/api/system.datetime?view=net-5.0#methods)