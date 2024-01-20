---
title:                "Obtendo a data atual"
date:                  2024-01-20T15:16:14.130904-07:00
html_title:           "Bash: Obtendo a data atual"
simple_title:         "Obtendo a data atual"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/ruby/getting-the-current-date.md"
---

{{< edit_this_page >}}

## O Que é & Por Que?
Obter a data atual em Ruby significa acessar informações de tempo reais do sistema. Programadores fazem isso para registros, funções de tempo e para funcionalidades que dependem de data e hora.

## Como Fazer:
Obter a data e hora atual no Ruby é simples. Vamos usar a biblioteca `Date` e `Time`:

```Ruby
require 'date'
require 'time'

# Obtendo a data atual
data_atual = Date.today
puts data_atual   # Exemplo de saída: 2023-04-12

# Obtendo a hora atual
hora_atual = Time.now
puts hora_atual   # Exemplo de saída: 2023-04-12 10:23:45 +0300
```

## Mergulho Profundo:
Ruby tem várias formas de trabalhar com datas e horas. Historicamente, a classe `Time` fazia o trabalho básico. No entanto, Ruby introduziu a biblioteca `Date` e 'DateTime' para lidar especificamente com datas.

Outras alternativas incluem o uso de bibliotecas de terceiros como `ActiveSupport` do Rails, que adicionam métodos ainda mais úteis. Mas para muitos propósitos, `Date` e `Time` nativos do Ruby são suficientes.

Quando usamos `Date.today` ou `Time.now`, estamos criando objetos baseados no relógio interno do sistema onde o Ruby está rodando. Ali, `Date` lida apenas com datas, enquanto `Time` pode lidar com data e hora, incluindo segundos e frações de segundos.

## Veja Também:
Para mais informações sobre datas e horas em Ruby, confira os seguintes recursos:
- [Documentação do Ruby para a classe Time](https://ruby-doc.org/core-2.7.0/Time.html)
- [Documentação do Ruby para a classe Date](https://ruby-doc.org/stdlib-2.7.0/libdoc/date/rdoc/Date.html)
- [Active Support Core Extensions](https://edgeguides.rubyonrails.org/active_support_core_extensions.html#extensions-to-time) para ver como o Rails estende as funcionalidades de datas e horas.