---
title:    "Ruby: Converter uma data em uma string"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/ruby/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Por que converter uma data em uma string?

Em programação, muitas vezes nos deparamos com a necessidade de formatar datas para que sejam legíveis e compreensíveis para os usuários. Converter uma data em uma string é uma forma de apresentar informações de data de uma maneira mais amigável para o usuário final.

## Como fazer isso?

Converter uma data em uma string pode ser feito de várias maneiras em Ruby. Uma das maneiras mais simples é usando o método `strftime`, que permite que você especifique um formato para a data.

```
data = Time.new(2020, 9, 22)
data_formatada = data.strftime("%d/%m/%Y")
puts data_formatada # Saída: 22/09/2020
```

Além disso, você também pode usar o método `to_s` para converter uma data em sua representação de string padrão.

```
data = Time.new(2020, 9, 22)
data_string = data.to_s 
puts data_string # Saída: 2020-09-22 00:00:00 +0300
```

Existem muitas outras maneiras de converter datas em strings em Ruby, por isso é importante explorar e descobrir qual método funciona melhor para a sua necessidade específica.

## Mergulho profundo

Ao converter uma data em uma string, existem várias coisas a serem consideradas. Uma delas é o fuso horário - certifique-se de especificar o fuso horário correto ao criar uma nova data para evitar erros de conversão.

Além disso, é importante se familiarizar com a sintaxe de formatação de datas em Ruby, pois diferentes métodos podem exigir formatos de data específicos.

Outra coisa a ter em mente é que, ao converter uma data em uma string, você está essencialmente perdendo a funcionalidade de data - você não poderá mais realizar operações de data e hora com a string resultante. Portanto, é importante manter a data original em uma variável e usar a string convertida apenas para apresentação.

## Veja também

- https://ruby-doc.org/core-2.7.2/Time.html#method-i-strftime
- https://ruby-doc.org/core-2.7.2/Time.html#method-i-to_s