---
title:                "Analisando uma data a partir de uma string"
html_title:           "PowerShell: Analisando uma data a partir de uma string"
simple_title:         "Analisando uma data a partir de uma string"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/ruby/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## O Que & Porquê?

Analisar uma data a partir de uma string significa converter uma sequência de texto representando uma data em um objeto de data. Os programadores fazem isso para manipular, comparar ou calcular datas de maneira eficiente no código.

## Como Fazer:

Aqui está um exemplo de análise da data de uma string com o método `parse` fornecido pelo módulo `Date` do Ruby:

```Ruby
require 'date'

string_de_data = "2021-08-25"
data = Date.parse(string_de_data)

puts data
```

Esta é a saída que você obterá:

```Ruby
2021-08-25
```

## Mergulhe Mais Fundo

Durante o início do desenvolvimento do software, a interpretação das datas foi um desafio devido aos diferentes formatos de data usados ao redor do mundo. Por esse motivo, os módulos de manipulação de data são uma parte essencial de muitas linguagens de programação hoje.

Alternativamente, pode-se usar a função `strptime` se o formato da string de data não estiver no formato padrão YYYY-MM-DD. Aqui está um exemplo:

```Ruby
require 'date'

string_de_data = "25-08-2021"
formato = "%d-%m-%Y"

data = Date.strptime(string_de_data, formato)

puts data
```

Internamente, o método `parse` analisa a string e tenta entender o formato da data, enquanto `strptime` precisa do formato especificado.

## Veja Também

Para mais informações sobre os métodos de tratamento de data em Ruby, verifique estes links:

- Documentação oficial do Ruby para a classe 'Date': https://ruby-doc.org/standard-2.5.1/libdoc/date/rdoc/Date.html
- Como usar `strptime` para analisar uma data: https://www.justinweiss.com/articles/3-ways-to-parse-a-date-in-ruby/