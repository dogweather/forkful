---
title:                "Obtendo a data atual"
date:                  2024-02-03T19:10:39.345310-07:00
model:                 gpt-4-0125-preview
simple_title:         "Obtendo a data atual"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/ruby/getting-the-current-date.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## O Que & Por Quê?
Obter a data atual é uma tarefa essencial em quase qualquer empreitada de programação, desde registrar atividades em uma aplicação até gerar relatórios com carimbos de data. Em Ruby, isso pode ser facilmente alcançado usando a biblioteca padrão, simplificando operações que envolvem datas.

## Como fazer:
A biblioteca padrão do Ruby inclui as classes `Date` e `Time` para manipular datas e horas. Aqui está como obter a data atual:

```ruby
require 'date'

current_date = Date.today
puts current_date
```

Saída de exemplo: 
```
2023-04-12
```

Para incluir a hora com a data, a classe `Time` do Ruby é mais adequada:

```ruby
current_time = Time.now
puts current_time
```

Saída de exemplo: 
```
2023-04-12 14:33:07 +0200
```

Se você precisar de mais funcionalidades, como gerenciamento de fuso horário, talvez queira usar uma gem de terceiros como a `ActiveSupport` (parte do Rails, mas pode ser usada de forma independente).

Primeiro, adicione `activesupport` ao seu Gemfile e execute `bundle install`:

```ruby
gem 'activesupport'
```

Depois, use-a para manipular fusos horários:

```ruby
require 'active_support/time'

Time.zone = 'Eastern Time (US & Canada)'  # Defina seu fuso horário desejado
current_time_with_zone = Time.zone.now
puts current_time_with_zone
```

Saída de exemplo:
```
Qua, 12 Abr 2023 08:33:07 EDT -04:00
```
