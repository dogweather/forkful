---
title:    "C#: Convertendo uma data em string"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/c-sharp/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Por que converter uma data em string?

Converter uma data em string é uma tarefa comum em programação, pois permite que as datas sejam exibidas de uma forma mais compreensível e legível para os usuários. Além disso, ao converter uma data em string, é possível formatá-la de acordo com as preferências do usuário ou as necessidades do sistema.

## Como fazer:

Para converter uma data em string em C#, é necessário utilizar o método `ToString()` da classe `DateTime`. A seguir, um exemplo de código e saída de uma data formatada para o padrão brasileiro:

```C#
DateTime data = new DateTime(2021, 10, 31);
string dataFormatada = data.ToString("dd/MM/yyyy");
Console.WriteLine(dataFormatada); // saída: 31/10/2021
```

É possível também utilizar diferentes formatos de data e hora, como exemplificado abaixo:

```C#
DateTime data = new DateTime(2021, 10, 31, 12, 30, 0);
string dataFormatada = data.ToString("dd 'de' MMMM 'de' yyyy 'às' HH:mm:ss");
Console.WriteLine(dataFormatada); // saída: 31 de outubro de 2021 às 12:30:00
```

## Uma visão mais detalhada:

Ao converter uma data em string, é importante observar que existem diferentes formatos que podem ser utilizados. Além do formato de data e hora padrão do sistema, é possível utilizar os chamados "códigos de formato" para definir uma formatação personalizada.

Por exemplo, ao utilizar o código "dddd" em uma data, o dia da semana será escrito por extenso de acordo com o idioma do sistema. Já o código "mmmm" irá exibir o mês por extenso.

Outro ponto importante a ser destacado é o uso do método `Parse()` para converter uma string em data. Este método é utilizado quando se deseja converter uma data no formato string para o formato padrão de data e hora do sistema, possibilitando assim a manipulação e cálculos com a data.

## Veja também:

- [Documentação oficial do método `ToString()` da classe `DateTime` em C#](https://docs.microsoft.com/pt-br/dotnet/api/system.datetime.tostring)
- [Guia completo de formatos de data e hora em C#](https://docs.microsoft.com/pt-br/dotnet/standard/base-types/custom-date-and-time-format-strings)
- [Tutorial sobre como converter uma string em data em C#](https://www.devmedia.com.br/trabalhando-com-datas-e-horas-em-csharp/25204)