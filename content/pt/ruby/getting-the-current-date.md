---
title:                "Ruby: Obtendo a data atual"
programming_language: "Ruby"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/ruby/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Por que precisamos da data atual?

É muito comum em qualquer linguagem de programação a necessidade de obter a data atual. Isso pode ser útil para diversas situações, como registros de eventos, análises de dados, entre outros. Neste artigo, vamos explorar como obter a data atual em Ruby.

## Como fazer isso em Ruby

Em Ruby, podemos utilizar o objeto `DateTime` para obter a data e hora atual. Para isso, precisamos primeiro exigir `date` como uma biblioteca em nosso código:

```Ruby
require 'date'
```

Em seguida, podemos usar o método `now` do objeto `DateTime` para obter a data e hora atuais:

```Ruby
DateTime.now
```

Podemos também especificar o formato em que desejamos que a data seja exibida, utilizando o método `strftime`. Por exemplo, se quisermos exibir a data atual no formato dd/mm/aaaa, podemos fazer o seguinte:

```Ruby
DateTime.now.strftime("%d/%m/%Y")
```

Isso irá resultar em uma string contendo a data atual no formato desejado, como por exemplo: 29/07/2021.

## Aprofundando mais sobre a obtenção da data atual

Em Ruby, o objeto `DateTime` é derivado do objeto `Date`, que por sua vez é derivado do objeto `Object`. Isso significa que ele herda diversos métodos e propriedades dos objetos pais.

Além disso, é importante ressaltar que ao utilizar o método `now`, obtemos a data e hora atual com base no fuso horário do sistema em que o código está sendo executado. Se quisermos especificar um fuso horário diferente, podemos utilizar o método `at_time` e passar como parâmetro o fuso horário desejado.

Outro detalhe importante é que ao utilizar o método `strftime`, é possível especificar diversos formatos diferentes para a data e hora, como por exemplo: dia da semana, mês, ano, hora, entre outros.

# Veja também

- [Documentação oficial do Ruby sobre DateTime](https://ruby-doc.org/stdlib-1.9.3/libdoc/date/rdoc/DateTime.html)
- [Tutorial sobre manipulação de datas em Ruby](https://www.tutorialspoint.com/ruby/time_date.htm)
- [Exemplo prático de utilização do objeto DateTime em Ruby](https://www.rubyguides.com/2017/02/ruby-date-format/)

Esperamos que este artigo tenha sido útil e possa facilitar a obtenção da data atual em suas aplicações em Ruby. Divirta-se programando!