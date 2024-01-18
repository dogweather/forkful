---
title:                "Analisando uma data a partir de uma sequência de caracteres"
html_title:           "Ruby: Analisando uma data a partir de uma sequência de caracteres"
simple_title:         "Analisando uma data a partir de uma sequência de caracteres"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/ruby/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

O que e por que "parsing" uma data de uma string?

"Parsing" de uma data de uma string é o processo de extrair informações de uma string para obter uma data em um formato específico. Isso é útil para programadores que precisam lidar com dados de formato de data variados e precisam convertê-los em um formato padrão para processamento.

Como fazer:

```Ruby
require 'date'

# Convertendo uma string em formato de data válido usando o método 'parse' da classe 'Date'
Date.parse('15/04/2021')
# Output: #<Date: 2021-04-15 ((2459329j,0s,0n),+0s,2299161j)>

# Extraindo informações de uma string usando formatação de data
data = Date.strptime('20/10/2018', '%d/%m/%Y')
puts data.day
# Output: 20
puts data.month
# Output: 10
puts data.year
# Output: 2018
```

Mergulho profundo:

Antes do advento de linguagens de programação modernas, a leitura e manipulação de dados de data era uma tarefa extremamente complicada e propensa a erros. Hoje, existem várias bibliotecas e métodos nativos em linguagens de programação, como o método 'parse' em Ruby, que tornam o processo de parsing de data de strings muito mais fácil e eficiente.

Além disso, existem várias alternativas para realizar parsing de data, como a biblioteca 'Chronic' que permite interpretar expressões de tempo em linguagem natural. Também é importante ter cuidado ao lidar com datas em diferentes fusos horários e considerar o uso de bibliotecas como 'tzinfo' para evitar problemas comuns.

Veja também:

- Documentação do método 'parse' da classe 'Date': https://ruby-doc.org/stdlib-3.0.0/libdoc/date/rdoc/Date.html#method-i-parse
- Biblioteca 'Chronic' para parsing de datas em linguagem natural: https://github.com/mojombo/chronic
- Biblioteca 'tzinfo' para lidar com fusos horários: https://github.com/tzinfo/tzinfo