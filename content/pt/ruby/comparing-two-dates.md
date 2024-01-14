---
title:                "Ruby: Comparando duas datas"
programming_language: "Ruby"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/ruby/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Por que comparar duas datas no Ruby?

Comparar datas é uma tarefa comum em programação, especialmente quando lidamos com dados temporais. No Ruby, essa comparação pode ser feita de maneira fácil e eficiente, permitindo que nossos programas tomem decisões baseadas em datas específicas. Neste post, vamos explorar como comparar duas datas no Ruby e entender melhor por que isso é importante.

## Como comparar duas datas no Ruby?

Comparar duas datas no Ruby é bastante simples e direto. Para isso, podemos utilizar o método `#<=>`, que compara dois objetos e retorna um valor numérico indicando se o primeiro é menor, igual ou maior do que o segundo. Podemos utilizar esse método diretamente com objetos do tipo `Date` ou `DateTime`.

```Ruby
date1 = Date.new(2021, 1, 1)
date2 = Date.new(2020, 12, 31)

puts date1 <=> date2 # Output: 1
```

Neste exemplo, o valor 1 é retornado porque a primeira data é maior do que a segunda. Se trocarmos as datas, o valor retornado será -1, indicando que a primeira data é menor. Caso as datas sejam iguais, o valor retornado será 0.

Este método também pode ser usado em conjunto com operadores de comparação, como `<` e `>`, o que facilita a escrita de condições lógicas em nosso código.

```Ruby
if date1 < date2
  puts "A segunda data é anterior à primeira."
end
```

## Deep Dive

No Ruby, as datas são tratadas como objetos e possuem diversos métodos úteis para realizar operações com elas. Por exemplo, podemos usar o método `#strftime` para formatar uma data em uma string de acordo com um padrão específico.

```Ruby
date = Date.new(2021, 7, 4)

puts date.strftime("%d/%m/%Y") # Output: 04/07/2021
puts date.strftime("%B, %Y") # Output: July, 2021
```

Além disso, existem também os métodos `#next` e `#prev` que nos permitem obter a próxima ou a data anterior à data atual, respectivamente. Esses métodos são muito úteis em situações em que precisamos iterar por um intervalo de datas específico.

## Veja também

- [Documentação oficial do Ruby sobre a classe Date](https://ruby-doc.org/stdlib-2.7.2/libdoc/date/rdoc/Date.html)
- [Vídeo tutorial sobre como comparar datas no Ruby](https://www.youtube.com/watch?v=8VvebXkWJdU)
- [Exemplos de código para praticar a comparação de datas no Ruby](https://learnruby.com/examples/date.html)