---
title:                "Obtendo a data atual"
html_title:           "C#: Obtendo a data atual"
simple_title:         "Obtendo a data atual"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c-sharp/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Por que

Você pode estar se perguntando: por que precisamos obter a data atual no nosso programa em C#? Existem várias razões pelas quais isso pode ser útil. Talvez você queira saber a data exata em que o seu programa está sendo executado para fins de registro ou para exibir em uma interface para o usuário. Ou talvez você precise comparar duas datas para realizar alguma lógica no seu programa. De qualquer forma, obter a data atual é uma tarefa comum em muitos projetos de programação.

## Como fazer

Agora que entendemos por que precisamos obter a data atual, aqui estão algumas maneiras de fazer isso usando C#.

### Utilizando DateTime.Now

A maneira mais simples de obter a data atual em C# é usando a classe DateTime e o método estático Now. Veja um exemplo abaixo:

```C#
DateTime dataAtual = DateTime.Now;
Console.WriteLine($"A data atual é: {dataAtual}");
```

A saída deste código será algo como "A data atual é: 19/09/2021 15:00:00". Isso pode variar de acordo com a data e hora atual em que você está executando o código.

### Formatando a data

Você também pode formatar a data obtida usando o método ToString e passando um formato específico como parâmetro. Por exemplo:

```C#
DateTime dataAtual = DateTime.Now;
string dataFormatada = dataAtual.ToString("dd/MM/yyyy");
Console.WriteLine($"A data atual é: {dataFormatada}");
```

A saída desse código seria apenas a data atual formatada no padrão dia/mês/ano, no formato de texto "19/09/2021".

### Obtendo o dia da semana

Se você precisar apenas do dia da semana atual, pode usar o método DayOfWeek da classe DateTime:

```C#
DateTime dataAtual = DateTime.Now;
Console.WriteLine($"Hoje é {dataAtual.DayOfWeek}");
```

A saída seria algo como "Hoje é domingo".

## Profundando

Se você quiser se aprofundar mais no assunto, existem outras classes e métodos que podem ser úteis para obter informações mais detalhadas sobre a data atual. Por exemplo, a classe GregorianCalendar pode ser usada para manipular datas de acordo com o calendário gregoriano.

## Veja também

- [Documentação da classe DateTime no C#](https://docs.microsoft.com/pt-br/dotnet/api/system.datetime?view=net-5.0)
- [Utilizando o método Now para obter a data atual em C#](https://docs.microsoft.com/pt-br/dotnet/api/system.datetime.now?view=net-5.0)