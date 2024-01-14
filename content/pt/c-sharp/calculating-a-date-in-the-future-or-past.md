---
title:                "C#: Calculando uma data no futuro ou passado"
programming_language: "C#"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c-sharp/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# Porquê

Às vezes, precisamos saber que dia da semana será daqui a um mês ou em qual data caia o Natal no próximo ano. Ou talvez, precisamos calcular quantos dias se passaram desde o nosso aniversário até hoje. Nesses casos, é útil saber como calcular uma data no futuro ou no passado usando programação. Isso nos poupa tempo e evita erros manuais.

# Como Fazer

Para calcular uma data no futuro ou no passado, primeiro precisamos criar uma instância da classe `DateTime`. Vamos criar uma variável `dataAtual`, que irá armazenar a data de hoje:

```C#
DateTime dataAtual = DateTime.Today;
```

Agora, suponha que queremos saber qual será a data daqui a 10 dias. Podemos usar o método `AddDays` para somar 10 dias à nossa data atual:

```C#
DateTime dataNoFuturo = dataAtual.AddDays(10);
```

Da mesma forma, se quisermos saber qual foi a data 20 dias atrás, podemos usar o método `AddDays` novamente, desta vez com um valor negativo:

```C#
DateTime dataNoPassado = dataAtual.AddDays(-20);
```

Além disso, também podemos usar os métodos `AddMonths` e `AddYears` para calcular datas em um futuro ou passado mais distante. Por exemplo, para saber qual será a data daqui a 6 meses, podemos fazer:

```C#
DateTime dataNoFuturoLongoPrazo = dataAtual.AddMonths(6);
```

E, para saber qual data cai daqui a 3 anos, podemos fazer:

```C#
DateTime dataNoFuturoExtremo = dataAtual.AddYears(3);
```

Ao executar o código acima, obteremos as datas corretas no futuro ou no passado, dependendo da operação realizada.

# Profundidade

O cálculo de datas no futuro ou no passado é possível graças ao fato de que a classe `DateTime` possui métodos para adicionar e subtrair dias, meses e anos. Além disso, também podemos obter informações detalhadas sobre uma data, como dia da semana, dia do ano e até mesmo a diferença em dias entre duas datas.

Para saber mais sobre as funcionalidades da classe `DateTime`, consulte a documentação oficial da Microsoft: https://docs.microsoft.com/pt-br/dotnet/api/system.datetime?view=net-5.0

# Veja Também

- https://docs.microsoft.com/pt-br/dotnet/api/system.datetime.adddays?view=net-5.0
- https://www.c-sharpcorner.com/blogs/add-days-to-date-in-c-sharp1
- https://www.devmedia.com.br/trabalhando-com-datas-no-csharp/22297