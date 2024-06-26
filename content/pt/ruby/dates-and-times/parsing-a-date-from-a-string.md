---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:15:13.575796-07:00
description: "Como Fazer: No Ruby, a biblioteca padr\xE3o oferece maneiras diretas\
  \ de analisar datas a partir de strings usando as classes `Date` e `DateTime`. Veja\
  \ como\u2026"
lastmod: '2024-03-13T22:44:47.104642-06:00'
model: gpt-4-0125-preview
summary: "No Ruby, a biblioteca padr\xE3o oferece maneiras diretas de analisar datas\
  \ a partir de strings usando as classes `Date` e `DateTime`."
title: Analisando uma data a partir de uma string
weight: 30
---

## Como Fazer:
No Ruby, a biblioteca padrão oferece maneiras diretas de analisar datas a partir de strings usando as classes `Date` e `DateTime`. Veja como fazer isso usando métodos integrados do Ruby:

```ruby
require 'date'

# Analisar uma data a partir de uma string
date_string = "2023-04-01"
parsed_date = Date.parse(date_string)
puts parsed_date
# => 2023-04-01

# DateTime para uma representação mais detalhada do tempo
datetime_string = "2023-04-01T15:30:45+00:00"
parsed_datetime = DateTime.parse(datetime_string)
puts parsed_datetime
# => 2023-04-01T15:30:45+00:00
```

Para ter mais controle ou lidar com formatos que o `parse` pode não entender diretamente, você pode usar `strptime` (analisa string de tempo), especificando o formato explicitamente:

```ruby
# Usando strptime para formatos personalizados
custom_date_string = "01-04-2023"
parsed_date_custom = Date.strptime(custom_date_string, '%d-%m-%Y')
puts parsed_date_custom
# => 2023-04-01
```

### Usando bibliotecas de terceiros:
Embora as capacidades integradas do Ruby sejam poderosas, às vezes você pode preferir bibliotecas de terceiros para recursos adicionais ou sintaxe mais simples. Uma escolha popular é a gem `Chronic` para análise de linguagem natural:

1. Primeiro, adicione Chronic ao seu Gemfile e execute `bundle install`:
```ruby
gem 'chronic'
```

2. Então, use-o assim:
```ruby
require 'chronic'

parsed_chronic = Chronic.parse('próxima terça-feira')
puts parsed_chronic
# A saída variará dependendo da data atual; assume a análise em 2023-04-01
# => 2023-04-04 12:00:00 +0000
```

`Chronic` é muito útil para entrada de usuário, pois pode entender uma ampla gama de formatos de data em linguagem natural, tornando-o uma ferramenta poderosa para aplicações que requerem entrada de data flexível.
