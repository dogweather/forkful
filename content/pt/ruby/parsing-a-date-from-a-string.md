---
title:                "Analisando uma data a partir de uma string"
date:                  2024-01-20T15:38:28.619073-07:00
html_title:           "Arduino: Analisando uma data a partir de uma string"
simple_title:         "Analisando uma data a partir de uma string"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/ruby/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Analisar uma data a partir de uma string significa converter o texto para um formato de data que o Ruby possa entender e manipular. Isso é essencial porque datas são frequentemente armazenadas e transferidas em texto, e você precisa trabalhar com elas em programas para fazer coisas como comparações de datas, cálculos de tempo, e agendamentos.

## How to:
Para analisar datas no Ruby, você geralmente vai trabalhar com a classe `Date` ou `DateTime`. Aqui estão alguns exemplos práticos:

```ruby
require 'date'

# Analisando uma data no formato ISO 8601
data_string = '2023-04-08'
data = Date.iso8601(data_string)
puts data
# Saída: 2023-04-08

# Analisando formatos personalizados
data_string_custom = '08/04/2023'
data_custom = Date.strptime(data_string_custom, '%d/%m/%Y')
puts data_custom
# Saída: 2023-04-08

# Time também pode ser usado para incluir tempo
tempo_string = '08/04/2023 14:30'
tempo = DateTime.strptime(tempo_string, '%d/%m/%Y %H:%M')
puts tempo
# Saída: 2023-04-08T14:30:00+00:00
```

## Deep Dive
Analisar strings de datas é algo que evoluiu com o tempo. Antigamente, a manipulação de datas era mais trabalhosa e as bibliotecas padrão não eram tão robustas quanto hoje. No Ruby, antes da classe `Date` e `DateTime` se tornarem padrão na biblioteca de tempo, outros mecanismos como `Time` eram mais utilizados, mas eles tinham suas limitações, como uma faixa de datas mais restritas.

Existem alternativas ao uso do `Date` e `DateTime`, como a gem `Timecop` para viagens no tempo (em testes, claro) e a gem `Chronic` para análise de datas naturais (mas que está descontinuada no momento da escrita deste artigo). Contudo, para a maioria das tarefas, o uso das classes padrão é mais que suficiente.

Na implementação, é importante considerar o formato da data. A função `strptime` permite definir o formato para a análise da string, dando flexibilidade para lidar com diferentes formatos.

## See Also
Para mais informações sobre as classes `Date` e `DateTime`, confira a documentação oficial:

- Documentação do Ruby para a classe Date: [https://ruby-doc.org/stdlib-3.1.2/libdoc/date/rdoc/Date.html](https://ruby-doc.org/stdlib-3.1.2/libdoc/date/rdoc/Date.html)
- Documentação do Ruby para a classe DateTime: [https://ruby-doc.org/stdlib-3.1.2/libdoc/date/rdoc/DateTime.html](https://ruby-doc.org/stdlib-3.1.2/libdoc/date/rdoc/DateTime.html)

Para entender melhor a viagem no tempo em testes e como usar a gem Timecop:

- GitHub da gem Timecop: [https://github.com/travisjeffery/timecop](https://github.com/travisjeffery/timecop)

Apesar de não ser recomendada para novos projetos, a gem Chronic ainda é uma referência interessante:

- GitHub da gem Chronic (arquivado): [https://github.com/mojombo/chronic](https://github.com/mojombo/chronic)