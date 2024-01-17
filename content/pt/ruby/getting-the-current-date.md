---
title:                "Obtendo a data atual"
html_title:           "Ruby: Obtendo a data atual"
simple_title:         "Obtendo a data atual"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/ruby/getting-the-current-date.md"
---

{{< edit_this_page >}}

## O que & Por que?

Obter a data atual é uma tarefa comum em programação que envolve a obtenção da data atual do sistema em que o programa está sendo executado. Isso pode ser útil para registrar informações de data e hora em arquivos, manter registros de atualizações ou agendar certas tarefas no código.

## Como fazer:

Para obter a data atual em Ruby, podemos usar o método `Time.now`, que retorna um objeto Time representando a data e hora atual. Podemos então formatar o objeto para exibir a data em diferentes formatos usando o método `strftime` e passando um argumento de formatação. Por exemplo:

```
Ruby Time.now.strftime("%d/%m/%Y")
# saída: 05/10/2021
```

Podemos também usar o método `Date.today` para obter a data atual em formato de objeto Date. Isso pode ser útil ao lidar com cálculos e comparações de datas. Por exemplo:

```
Ruby Date.today.month
# saída: 10
```

## Mergulho profundo:

Obter a data atual é uma tarefa que tem sido feita pelos programadores há muito tempo. No entanto, antes do advento das linguagens de programação modernas, muitas vezes era necessário escrever código específico para cada sistema operacional ou máquina. Hoje em dia, a maioria das linguagens de programação tem métodos incorporados para facilitar essa tarefa.

Além dos métodos `Time.now` e `Date.today`, também há outras maneiras de obter a data atual em Ruby. Por exemplo, podemos usar a classe `DateTime`, que oferece mais opções de formatação e manipulação de datas. Além disso, podemos usar a biblioteca `Date.parse` para converter uma string em um objeto Date e, em seguida, formatá-lo usando o método `strftime`.

## Veja também:

- [Documentação oficial do Ruby sobre a classe Time](https://ruby-doc.org/core-3.0.2/Time.html)
- [Documentação oficial do Ruby sobre a classe Date](https://ruby-doc.org/stdlib-3.0.2/libdoc/date/rdoc/Date.html)
- [Tutorial sobre manipulação de datas em Ruby](https://www.rubyguides.com/2015/05/ruby-date-time-and-datetime/)
- [Mais informações sobre formatos de data em Ruby](https://apidock.com/ruby/DateTime/strftime)