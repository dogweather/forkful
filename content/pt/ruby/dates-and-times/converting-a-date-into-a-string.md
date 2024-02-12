---
title:                "Convertendo uma data em uma string"
aliases:
- /pt/ruby/converting-a-date-into-a-string.md
date:                  2024-01-20T17:37:35.463325-07:00
model:                 gpt-4-1106-preview
simple_title:         "Convertendo uma data em uma string"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/ruby/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## O Que & Porquê?

Converter uma data em uma string significa transformar um objeto de data, que o Ruby entende e manipula, em texto simples, que pode ser lido e usado por humanos ou outros sistemas. Fazemos isso para exibir datas de maneira compreensível em interfaces, armazenar em arquivos de texto ou integrar com sistemas que esperam datas em formato de texto.

## Como Fazer:

```Ruby
require 'date'

# Criar um objeto de data
data = Date.new(2023, 4, 1)

# Converter para string com to_s (formato padrão: YYYY-MM-DD)
data_string = data.to_s 
puts data_string
# Saída: 2023-04-01

# Formatação personalizada com strftime
data_formatada = data.strftime('%d/%m/%Y')
puts data_formatada
# Saída: 01/04/2023

# Outros exemplos de formatação
puts data.strftime('%B %d, %Y')  # Nome do mês, dia, ano
# Saída: April 01, 2023

puts data.strftime('%A, %d de %B de %Y')  # Dia da semana, dia, mês e ano por extenso
# Saída: Saturday, 01 de April de 2023
```

## Mergulho Profundo:

Converter datas em strings não é uma invenção moderna; é uma necessidade desde os primórdios da programação. Em Ruby, isso é alcançado com métodos como `to_s` e `strftime`, presentes desde as primeiras versões. O método `strftime` tem uma flexibilidade enorme permitindo uma variedade de formatos, a partir dos códigos de formatação específicos, que podem ser consultados na documentação do Ruby.

Alternativas ao `strftime` incluem o uso de gemas como o `time_format` ou bibliotecas de internacionalização como o `I18n` que ajudam na formatação de strings de datas com suporte a localização, algo especialmente útil em contextos multilíngues.

Quanto aos detalhes de implementação, cada chamada ao `strftime` efetivamente constrói a string de acordo com os tokens especificados, analisando as diferentes partes do objeto de data (como dia, mês, ano) e substituindo esses tokens pelos valores correspondentes formatados.

## Veja Também:

- [Listagem completa dos códigos de formatação do strftime](https://apidock.com/ruby/DateTime/strftime)
- [Guia para internacionalização I18n no Ruby on Rails](https://guides.rubyonrails.org/i18n.html)
