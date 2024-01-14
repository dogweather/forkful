---
title:    "Ruby: Comparando duas datas"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

## Por que comparar duas datas é importante no Ruby

Comparar duas datas é uma tarefa comum para programadores Ruby. Isso pode ser necessário para verificar se uma data é anterior ou posterior a outra, calcular a diferença de dias entre elas ou simplesmente para fins de ordenação. Neste artigo, discutiremos por que a comparação de datas é importante e como ela pode ser feita usando Ruby.

## Como comparar duas datas em Ruby

A comparação de datas em Ruby é feita através do operador "maior que" (>) e "menor que" (<). Para isso, é necessário converter as datas para o objeto Date do Ruby. Veja o exemplo abaixo:

```ruby
date_1 = Date.parse("2021-01-20")
date_2 = Date.parse("2021-01-25")

if date_1 < date_2
  puts "#{date_1} é anterior a #{date_2}"
elsif date_1 > date_2
  puts "#{date_1} é posterior a #{date_2}"
else
  puts "As datas são iguais"
end
```

A saída do código acima será:

```ruby
2021-01-20 é anterior a 2021-01-25
```

Além disso, também é possível calcular a diferença de dias entre duas datas utilizando o método "difference" do objeto Date, como mostrado abaixo:

```ruby
date_1 = Date.parse("2021-01-20")
date_2 = Date.parse("2021-01-25")

difference = date_2 - date_1
puts "A diferença de dias entre #{date_1} e #{date_2} é #{difference}"
```

A saída do código será:

```ruby
A diferença de dias entre 2021-01-20 e 2021-01-25 é 5
```

## Mais sobre a comparação de datas em Ruby

Além do operador maior e menor, a comparação de datas em Ruby também é possível através dos métodos "before?" e "after?", que retornam true ou false dependendo da relação entre as datas. Além disso, o objeto Date do Ruby também possui métodos para adicionar e subtrair dias, semanas, meses ou anos de uma data específica. Você pode conferir a documentação completa neste [link](https://ruby-doc.org/stdlib-2.7.2/libdoc/date/rdoc/Date.html).

## Veja também

Aqui estão alguns links úteis para continuar aprendendo sobre comparação de datas em Ruby:

[Documentação oficial do Ruby sobre o objeto Date](https://ruby-doc.org/stdlib-2.7.2/libdoc/date/rdoc/Date.html)

[Vídeo tutorial sobre comparando datas em Ruby](https://www.youtube.com/watch?v=JIkXUcG-ZLk)

[Tutorial sobre manipulação de datas em Ruby](https://www.digitalocean.com/community/tutorials/how-to-use-date-and-time-in-ruby-pt)

Esperamos que tenha gostado deste artigo e que ele tenha sido útil para suas aplicações em Ruby. Deixe sua opinião nos comentários e compartilhe com seus amigos programadores! #ruby #comparaçãodedatas #data #programação