---
title:                "Ruby: Convertendo uma data em uma string"
programming_language: "Ruby"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/ruby/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Por que

Ao trabalhar com programação em Ruby, é comum a necessidade de converter um objeto de data em uma string formatada. Isso pode ser útil ao apresentar informações em um formato específico para o usuário ou ao salvar dados em um arquivo. Neste artigo, vamos explorar como realizar essa conversão de forma simples e eficiente.

## Como fazer

Para converter uma data em uma string, podemos utilizar o método `strftime` em conjunto com um formato de data específico. Por exemplo, se quisermos converter a data atual em uma string no formato "dd/mm/yyyy", podemos fazer o seguinte:

```Ruby
puts Time.now.strftime("%d/%m/%Y")
```
O output seria "03/08/2021", considerando que hoje é dia 03 de agosto de 2021.

Podemos utilizar uma variedade de códigos de formatação para obter diferentes tipos de output. Alguns dos mais comuns são `%d` para o dia, `%m` para o mês e `%Y` para o ano. É possível consultar a documentação do Ruby para ver a lista completa de opções.

## Deep Dive

Além dos códigos de formatação, o método `strftime` também permite inserir strings estáticas no formato final. Por exemplo, se quisermos apresentar a data no formato "Dia dd de mês por extenso de yyyy", podemos fazer o seguinte:

```Ruby
puts Time.now.strftime("Dia %d de %B de %Y")
```
O output seria "Dia 03 de agosto de 2021".

Também é possível converter datas em outros fusos horários utilizando o método `gmtime`, que transforma a data no formato UTC (Universal Coordinated Time). Por exemplo, se quisermos apresentar a data atual no fuso horário de São Paulo, podemos fazer o seguinte:

```Ruby
puts Time.now.gmtime.strftime("%T UTC-03:00")
```

O output seria o horário atual em UTC-03:00, que corresponde ao horário de São Paulo.

## Veja também

- [Documentação oficial do método strftime](https://ruby-doc.org/core-3.0.1/Time.html#method-i-strftime)
- [Lista completa de códigos de formatação](https://ruby-doc.org/core-3.0.1/Time.html#method-i-strftime-label-Format+Directives)
- [Conversão de datas em diferentes fusos horários](https://www.rubyguides.com/2015/05/understanding-datetime-in-ruby/)