---
title:                "Comparando duas datas"
aliases:
- /pt/ruby/comparing-two-dates.md
date:                  2024-01-20T17:33:54.500063-07:00
model:                 gpt-4-1106-preview
simple_title:         "Comparando duas datas"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/ruby/comparing-two-dates.md"
---

{{< edit_this_page >}}

## O Que & Porquê?
Comparar duas datas significa verificar a diferença entre elas ou qual vem antes/depois. Programadores fazem isso para manipular períodos de tempo, agendar tarefas, validar prazos, ou simplesmente organizar dados cronologicamente.

## Como fazer:
```Ruby
require 'date'

# Criando duas datas para comparação
data1 = Date.new(2023, 4, 1)
data2 = Date.new(2023, 5, 1)

# Comparar datas (true se data1 for antes de data2)
data1_antes_data2 = data1 < data2
puts "Data1 é antes de Data2? #{data1_antes_data2}" # Saída: true

# Calcular a diferença em dias
diferenca_dias = (data2 - data1).to_i
puts "Diferença em dias: #{diferenca_dias}" # Saída: 30

# Verificar se as datas são iguais
datas_sao_iguais = data1 == data2
puts "As datas são iguais? #{datas_sao_iguais}" # Saída: false
```

## Mergulho Profundo:
Comparar datas é uma prática tão antiga quanto a própria noção de calendário. No Ruby, a classe `Date` e a classe `DateTime` são frequentemente utilizadas para lidar com datas e tempo. Antes do Ruby proporcionar essas classes no módulo `date`, programadores dependiam de bibliotecas de terceiros ou de manipulação manual de strings e timestamps.

Além de usar `<`, `>` e `==`, também podemos usar `<=>` (o operador de comparação) que retorna -1, 0 ou 1 dependendo se a primeira data é anterior, igual ou posterior à segunda. Esse operador é útil quando queremos ordenar arrays de datas, por exemplo.

Outra alternativa é usar as bibliotecas de terceiros, como o popular 'ActiveSupport' do framework Rails, que oferece métodos adicionais como `before?` e `after?` para facilitar a leitura.

Na implementação interna, o Ruby lida com datas convertendo-as para um formato de contador de dias desde uma data época, que é o 'Day Zero' em 1 de janeiro do ano 4713 A.C. Isso permite calcular diferenças de forma absoluta, evitando vários problemas de fuso horário e de calendário.

## Veja Também:
- Documentação oficial da classe de [Date do Ruby](https://ruby-doc.org/stdlib-3.0.0/libdoc/date/rdoc/Date.html).
- Ruby Guides sobre [trabalhar com datas e tempo em Ruby](https://www.rubyguides.com/2015/12/ruby-time/).
- ActiveSupport e suas utilidades para [datas e tempo](https://api.rubyonrails.org/classes/ActiveSupport/TimeWithZone.html).
