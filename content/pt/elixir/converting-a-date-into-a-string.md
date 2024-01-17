---
title:                "Convertendo uma data em uma string."
html_title:           "Elixir: Convertendo uma data em uma string."
simple_title:         "Convertendo uma data em uma string."
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elixir/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## O que e por que?

Converter uma data em uma string é um processo comum na programação, onde uma data é transformada em uma sequência de caracteres legível para os seres humanos. Isso é feito para facilitar a visualização e compreensão da data, que pode ser armazenada em um formato diferente no código.

## Como fazer:

```
Elixir Date.to_string!(~D[2020-01-01])
```

Saída:
```
"2020-01-01"
```

```
Elixir Date.to_string!(~D[2020-01-01], format: "{YYYY}-{MM}-{DD}")
```

Saída:
```
"2020-01-01"
```

## Profundidade:

A conversão de uma data em uma string é uma tarefa bastante simples em Elixir, graças ao módulo `Date` da biblioteca padrão. Isso permite que os programadores escolham o formato em que desejam exibir a data, usando a função `to_string!` e passando uma opção de formato.

Outra alternativa para converter uma data em uma string é usando o módulo `Calendar`, que possui mais opções de formatação e suporta diferentes calendários, como o calendário islâmico e hebraico.

Internamente, a conversão de uma data em uma string é feita através de operações matemáticas para obter o ano, mês e dia correspondentes e depois juntando-os em uma string formatada. Isso é feito de forma otimizada para garantir a eficiência e precisão.

## Veja também:

- [Documentação do módulo `Date` em Elixir](https://hexdocs.pm/elixir/Date.html)
- [Documentação do módulo `Calendar` em Elixir](https://hexdocs.pm/elixir/Calendar.html)