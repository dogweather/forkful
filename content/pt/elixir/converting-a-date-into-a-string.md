---
title:                "Elixir: Convertendo uma data em uma string"
simple_title:         "Convertendo uma data em uma string"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elixir/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

# Por que converter uma data em uma string em Elixir?

Converter uma data em uma string pode ser útil em muitas situações, como exibição de datas em formato legível ou armazenamento de dados em um banco de dados. Em Elixir, existem várias maneiras de realizar essa conversão, cada uma com suas próprias vantagens e desvantagens. Neste artigo, vamos explorar as diferentes opções para converter uma data em uma string em Elixir.

## Como fazer

Existem várias maneiras de converter uma data em uma string em Elixir, dependendo de suas necessidades específicas. Vamos dar uma olhada em algumas opções com exemplos de código e saídas.

### Utilizando o módulo `DateTime`

O módulo `DateTime` é uma das maneiras mais comuns de trabalhar com datas em Elixir. Para converter uma data em uma string, podemos usar o método `format`, que nos permite especificar o formato da string de saída. Por exemplo:

```elixir
DateTime.utc_now() |> DateTime.to_string("|MM/dd/yyyy|")
```
**Saída:** "|06/25/2021|"

Ao usar o caractere `|` como delimitador, podemos aplicar um formato personalizado à data. No exemplo acima, usamos `MM` para representar o mês e `dd` para representar o dia. Para obter uma lista completa de opções de formatação disponíveis, consulte a documentação oficial.

### Utilizando o módulo `Calendar`

Outra opção é utilizar o módulo `Calendar`, que possui uma função específica para converter uma data em uma string. O método `to_string` nos permite especificar o formato da saída, semelhante ao exemplo anterior.

```elixir
Calendar.local_time() |> Calendar.to_string("{YYYY}-{MM}-{DD}")
```
**Saída:** "2021-06-25"

Neste exemplo, usamos opções semelhantes, como `YYYY` para o ano, `MM` para o mês e `DD` para o dia. Você também pode adicionar outras opções, como `hh` para a hora ou `mm` para os minutos.

## Profundidade

Ao converter uma data em uma string, é importante ter em mente que diferentes opções de formatação podem resultar em resultados diferentes. Por exemplo, o formato `dd` pode retornar o dia com um único ou duplo dígito, dependendo da data. Além disso, certifique-se de estar trabalhando com o fuso horário correto ao utilizar o módulo `DateTime`.

## Veja também

- Documentação oficial do módulo DateTime: https://hexdocs.pm/elixir/DateTime.html 
- Documentação oficial do módulo Calendar: https://hexdocs.pm/elixir/Calendar.html 
- Formatação de datas e horas em Elixir: https://www.fluentinelixir.com/blog/date-and-time-formatting-in-elixir/