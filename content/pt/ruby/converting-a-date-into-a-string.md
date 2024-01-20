---
title:                "Convertendo uma data em uma string"
html_title:           "C++: Convertendo uma data em uma string"
simple_title:         "Convertendo uma data em uma string"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/ruby/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## O Que & Por Quê?

Converter uma data em string é a prática de transformar uma data formatada como uma string (cadeia de caracteres). Programadores fazem isso para facilitar a manipulação e apresentação dos dados.

## Como fazer:

Vamos à um exemplo prático em Ruby:

```Ruby
require 'date'

# Cria uma data
data_origem = Date.new(2020, 5, 17)

# Converte a data para string
string_data = data_origem.to_s

puts string_data
```

Saída:

```Ruby
"2020-05-17"
```
Neste exemplo, a data é convertida em uma string e impressa.

## Mergulho Profundo

A conversão de datas em strings não é um conceito novo; é uma prática comum em programação. Em Ruby, usamos o método `to_s` para essa operação, mas existem alternativas como a biblioteca 'time_format', que oferece uma funcionalidade semelhante com mais opções de formatação.

Quando chamamos `to_s` em uma data, Ruby chama internamente o método `strftime` com um argumento de formato de data padrão. Isso converte o objeto Data em string no formato 'AAAA-MM-DD'.

## Veja Também

Aqui estão algumas fontes úteis para você explorar mais:

- Documentação oficial do Ruby sobre a classe Date: [https://ruby-doc.org/stdlib-2.5.1/libdoc/date/rdoc/Date.html]
- Artigo detalhado sobre o método `strftime` em Ruby: [https://www.rubyguides.com/2015/12/ruby-time/]
- Documentação Ruby sobre a biblioteca 'time_format': [https://ruby-doc.org/stdlib-2.5.3/libdoc/time/rdoc/Time.html#method-i-strftime]