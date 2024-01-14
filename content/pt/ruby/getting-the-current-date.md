---
title:                "Ruby: Obtendo a data atual"
simple_title:         "Obtendo a data atual"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/ruby/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Porque

A programação é uma habilidade valiosa nos dias de hoje. Com o mercado de trabalho cada vez mais competitivo, aprender a programar está se tornando essencial para diversas áreas profissionais. E uma das tarefas comuns na programação é obter a data atual. Neste artigo, vamos mostrar como fazer isso usando a linguagem Ruby.

## Como Fazer

Para obter a data atual em Ruby, precisamos utilizar a classe Date, que está incluída no módulo padrão da linguagem. Podemos criar um objeto dessa classe usando o método estático `today` e, em seguida, formatá-lo para exibir a data da maneira desejada.

```
```Ruby
require 'date'
hoje = Date.today
puts hoje.strftime("%d/%m/%Y")
```
Saída: 29/05/2021

No código acima, primeiro importamos o módulo Date para ter acesso à classe. Em seguida, criamos um objeto "hoje" que contém a data atual. Finalmente, utilizamos o método `strftime` para formatar o objeto e exibimos o resultado com o `puts`. Podemos escolher diferentes formatos de exibição, como dia/mês/ano, mês/dia/ano ou até mesmo exibir o nome do mês em vez do número.

## Deep Dive

Além do método `today`, a classe Date também possui outros métodos úteis para trabalhar com datas. Por exemplo, podemos somar ou subtrair dias de uma data específica, ou ainda comparar duas datas para saber qual delas é mais recente. Também é possível criar objetos de data a partir de uma string ou especificando o dia, mês e ano separadamente.

Para uma lista completa de métodos e opções de formatação, é recomendado consultar a documentação oficial da classe Date em https://ruby-doc.org/stdlib-3.0.1/libdoc/date/rdoc/Date.html.

## Veja Também

- Documentação da classe Date em https://ruby-doc.org/stdlib-3.0.1/libdoc/date/rdoc/Date.html
- Tutorial de introdução à programação em Ruby em https://www.ruby-lang.org/pt/documentation/quickstart/
- Outros artigos sobre Ruby em https://www.rubyguides.com/