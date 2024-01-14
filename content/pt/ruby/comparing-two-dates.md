---
title:    "Ruby: Comparando duas datas"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/ruby/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Por que comparar duas datas é importante?

Ao trabalhar com programação, é comum lidar com dados de datas e entender como compará-los corretamente pode ser crucial para sua aplicação funcionar corretamente. Comparar duas datas pode ajudar a determinar qual delas é a mais recente, ou se as duas são iguais, por exemplo. Neste artigo, vamos discutir como comparar duas datas em Ruby e por que é importante dominar essa habilidade.

## Como comparar duas datas em Ruby

Em Ruby, podemos comparar duas datas usando o método `.compare`, que retorna um inteiro que indica a relação entre as duas datas. Vamos ver um exemplo de como usá-lo:

```ruby
date1 = Date.new(2021, 7, 15)
date2 = Date.new(2020, 7, 15)

puts date1.compare(date2) # retorna 1
```

Neste exemplo, estamos comparando duas datas, `date1` e `date2`. Como `date1` é posterior a `date2`, o método `.compare` retorna o valor 1. Mas e se as duas datas forem iguais? Vamos ver:

```ruby
date1 = Date.new(2021, 7, 15)
date2 = Date.new(2021, 7, 15)

puts date1.compare(date2) # retorna 0
```

Neste caso, o método `.compare` retorna o valor 0, pois ambas as datas são iguais. E se a data2 fosse posterior à data1? Vamos ver:

```ruby
date1 = Date.new(2020, 7, 15)
date2 = Date.new(2021, 7, 15)

puts date1.compare(date2) # retorna -1
```

Desta vez, como `date2` é a data mais recente, o método `.compare` retorna o valor -1. Isso nos dá uma maneira simples e eficiente de comparar duas datas em Ruby.

## Profundando na comparação de datas

Além do método `.compare`, também podemos usar os operadores de comparação `<`, `<=`, `==`, `>` e `>=` para comparar datas em Ruby. Por exemplo:

```ruby
date1 = Date.new(2021, 7, 15)
date2 = Date.new(2020, 7, 15)

puts date1 > date2 # retorna true
puts date1 == date2 # retorna false
puts date1 <= date2 # retorna false
```

Também é importante ter em mente que, ao comparar duas datas, a precisão da comparação depende da precisão do objeto de data usado. Por exemplo, se usarmos o objeto `DateTime` em vez de `Date`, a comparação incluirá as horas, minutos e segundos.

## Veja também

- [Documentação do método `.compare` em Ruby](https://ruby-doc.org/stdlib-2.7.1/libdoc/date/rdoc/Date.html#method-i-compare)
- [Como usar operadores de comparação em Ruby](https://www.tutorialspoint.com/ruby/ruby_operators.htm)