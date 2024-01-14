---
title:    "Ruby: Obtendo a data atual"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

## Por que
 Aprender como obter a data atual é essencial para qualquer programador de Ruby. Saber a data atual tem diversas aplicações em programas, como por exemplo em cálculos de prazos, geração de documentos ou simplesmente para impressão em tela.

## Como fazer
Para obter a data atual em Ruby, podemos utilizar o método `Date.today`, que retorna a data atual no formato `YYYY-MM-DD`.

```Ruby
require 'date'

data_atual = Date.today
puts data_atual
```
Output: 2021-06-21

Podemos também formatar a data de acordo com a nossa preferência, utilizando o método `strftime`.

```Ruby
require 'date'

data_atual = Date.today
puts data_atual.strftime("%d/%m/%Y")
```
Output: 21/06/2021

Ou ainda, podemos obter apenas o ano, mês ou dia atual utilizando os métodos `year`, `month` e `day`, respectivamente.

```Ruby
require 'date'

data_atual = Date.today
puts "Year: #{data_atual.year}"
puts "Month: #{data_atual.month}"
puts "Day: #{data_atual.day}"
```
Output:
```
Year: 2021
Month: 6
Day: 21
```

## Mergulho profundo
Além do método `Date.today`, Ruby possui outras formas de se obter a data atual. Uma delas é utilizando a classe `DateTime`, que nos permite incluir informações de hora e fuso horário na data atual.
Outra opção é utilizar o módulo `Time`, que também possui métodos para obter informações precisas sobre a data e hora atual.

## Veja também
- [Documentação do Ruby](https://ruby-doc.org/core-2.7.2/Date.html)
- [Tutorial de Ruby para iniciantes](https://www.ruby-lang.org/pt/documentation/quickstart/)