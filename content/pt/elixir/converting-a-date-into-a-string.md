---
title:    "Elixir: Convertendo uma data em uma string"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## Por que converter uma data em uma string?

Converter uma data em uma string pode ser útil em várias situações dentro da programação em Elixir. Algumas das possíveis razões para fazer isso incluem: exibição de informações para o usuário, formatação de dados para armazenamento em bancos de dados ou integração com outros sistemas que requerem uma string ao lidar com datas.

## Como fazer?

A conversão de uma data em uma string pode ser feita de maneira bastante simples em Elixir, utilizando a função `to_string/1` da biblioteca `DateTime`. Veja o exemplo abaixo:

```Elixir
date = DateTime.utc_now()
date_string = to_string(date)
IO.puts(date_string)
```
Este trecho de código irá imprimir a data atual em forma de string no formato padrão ISO8601: `2021-09-14T19:30:00Z`. No entanto, você também pode especificar o formato desejado utilizando a função `format/2` da mesma biblioteca, como mostrado no exemplo abaixo:

```Elixir
date = DateTime.utc_now()
date_string = DateTime.to_string(date, "{WDshort}, {Mshort} {d}, {yyyy}")
IO.puts(date_string)
```

Este trecho de código irá imprimir a data atual em forma de string no formato "ter, set 14, 2021": `ter, set 14, 2021`.

## Profundando mais

As funções `to_string/1` e `format/2` são apenas algumas das opções que a biblioteca `DateTime` oferece para trabalhar com datas em forma de string. Além disso, você também pode utilizar a biblioteca `Timex`, que possui uma série de funções específicas para lidar com formatação de datas e horas.

Por exemplo, a biblioteca `Timex` oferece a função `format/2` que pode ser utilizada da seguinte forma:

```Elixir
date = Timex.now()
date_string = Timex.format(date, "{Mshort} {d}, {yyyy}")
IO.puts(date_string)
```

Este trecho de código irá imprimir a data atual em forma de string no formato "set 14, 2021": `set 14, 2021`.

## Veja também

- Documentação da biblioteca `DateTime`: https://hexdocs.pm/elixir/DateTime.html
- Documentação da biblioteca `Timex`: https://hexdocs.pm/timex/Timex.html
- Exemplos de formatação de datas com `Timex`: https://medium.com/@buntine/oh-moments-ecdfa2d232cd