---
title:    "C#: Obtendo a data atual."
keywords: ["C#"]
---

{{< edit_this_page >}}

Olá leitores de programação C#! Seja bem-vindo a mais um post do nosso blog.

## Por que

Neste artigo, vamos falar sobre como obter a data atual em C# e por que isso pode ser útil em suas aplicações. A data atual é um dado bastante importante em muitos programas, pois pode ser usada para registrar quando uma transação foi realizada, para gerar relatórios diários ou até mesmo para exibir a data no formato correto para o usuário.

## Como fazer

Para obter a data atual em C#, utilizamos a classe `DateTime` e seu método `Now()`. Vamos ver um exemplo prático:

```C#
DateTime dataAtual = DateTime.Now;
Console.WriteLine($"A data atual é: {dataAtual}");
```

Ao executar este código, a saída será: `A data atual é: 20/09/2021 16:38:42`. Como podemos ver, o método `Now()` retorna um objeto `DateTime` com a data e hora atuais.

Podemos usar também métodos específicos da classe `DateTime` para obter apenas parte da data, como o dia, mês ou ano. Por exemplo, para exibir apenas a data atual no formato `dd/MM/yyyy`, podemos fazer o seguinte:

```C#
DateTime dataAtual = DateTime.Now;
Console.WriteLine($"A data atual é: {dataAtual.ToString("dd/MM/yyyy")}");
```

Isso resultará em: `A data atual é: 20/09/2021`.

## Mergulho profundo

A classe `DateTime` permite uma ampla gama de manipulações de data, como adicionar ou subtrair dias, meses ou anos, comparar datas e converter entre diferentes formatos. Além disso, também é possível trabalhar com fusos horários e horário de verão.

Uma funcionalidade interessante é a possibilidade de criar datas personalizadas com o método `Parse()`, que recebe uma string com a data e retorna um objeto `DateTime`. Por exemplo:

```C#
DateTime outraData = DateTime.Parse("25/12/2021");
Console.WriteLine($"A outra data é: {outraData.ToString("dd/MM/yyyy")}");
```

A saída será: `A outra data é: 25/12/2021`.

## Veja também

Aqui estão alguns links úteis para aprender mais sobre o uso de datas em C#:

- [Documentação oficial da classe DateTime](https://docs.microsoft.com/pt-br/dotnet/api/system.datetime?view=net-5.0)
- [Manipulação de datas em C#](https://www.devmedia.com.br/trabalhando-com-o-objeto-datetime-em-csharp/25497)

Esperamos que este artigo tenha sido útil para você entender como obter a data atual em C#. Fique ligado no nosso blog para mais conteúdos interessantes sobre programação! Até a próxima.