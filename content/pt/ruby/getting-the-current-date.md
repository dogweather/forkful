---
title:                "Obtendo a data atual"
html_title:           "C: Obtendo a data atual"
simple_title:         "Obtendo a data atual"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/ruby/getting-the-current-date.md"
---

{{< edit_this_page >}}

## O Que & Porquê?

Obter a data atual significa acessar e utilizar a data de hoje de acordo com o sistema operacional do seu computador. É uma tarefa comum em programação para marcar eventos, gerar registros de atividades ou mesmo controlar processos que exigem informação temporal.

## Como Fazer:

A maneira mais simples de obter a data atual em Ruby é utilizando o método `Date.today` da biblioteca padrão de Ruby (stdlib) "date". Aqui está um exemplo:

```Ruby
require 'date'

hoje = Date.today

puts "A data de hoje é #{hoje}"
```

Ao executar o código acima, você verá uma saída semelhante à seguinte:

```Ruby
A data de hoje é 2021-12-29
```

## Mergulhando Mais Fundo: 

Historicamente, Ruby sempre simplificou o trabalho com datas e horas. Assim, se você está utilizando uma versão mais antiga de Ruby, `Time.now.to_date` fará o mesmo que `Date.today`.

Existem algumas alternativas dependendo dos detalhos de implementação que você precisa. Se você deseja a data e a hora exatas agora, use `DateTime.now` ou `Time.now`. 

Observe que os objetos de tempo em Ruby são diferentes dos objetos de data -- eles incluem informações sobre a hora, minutos e segundos, além de fuso horário.

## Veja Também:

- [Documentação oficial Ruby para a classe Date](https://ruby-doc.org/stdlib-2.5.1/libdoc/date/rdoc/Date.html)
- [Documentação oficial Ruby para a classe Time](https://ruby-doc.org/stdlib-2.5.1/libdoc/time/rdoc/Time.html)
- [Um ótimo post no Stack Overflow comparando Date, Time e DateTime em Ruby](https://stackoverflow.com/questions/5941612/what-is-the-difference-between-date-time-and-datetime-in-ruby)