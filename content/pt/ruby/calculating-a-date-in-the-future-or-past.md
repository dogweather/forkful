---
title:                "Ruby: Calculando uma data no futuro ou passado"
programming_language: "Ruby"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/ruby/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# Por que calcular uma data no futuro ou passado?

Muitas vezes em programação, precisamos trabalhar com datas e muitas vezes precisamos calcular uma data específica no futuro ou no passado. Isso pode ser necessário para criar lembretes, agendar tarefas ou fazer cálculos de prazos. Felizmente, a linguagem Ruby tem algumas ótimas ferramentas para nos ajudar a calcular essas datas.

## Como fazer isso

Para calcular uma data no futuro ou passado em Ruby, podemos usar o método `+` e passar como argumento um número de dias que desejamos adicionar ou subtrair da data atual. Vamos dar uma olhada em um exemplo:

```Ruby
future_date = Time.now + 7 # adicionando 7 dias à data atual
puts future_date.strftime("%d/%m/%Y") # output: 14/09/2021
```

Neste exemplo, usamos o método `strftime` para formatar nossa data de acordo com o padrão de dia/mês/ano. É importante observar que, além de dias, também podemos adicionar meses e anos à data atual usando o método `+`.

No entanto, se quisermos calcular uma data em específico no futuro ou passado, podemos usar o método `parse` da classe `Date`. Vamos ver um exemplo:

```Ruby
past_date = Date.parse("01/01/2021") - 30 # subtraindo 30 dias da data especificada
puts past_date.strftime("%d/%m/%Y") # output: 02/12/2020
```

Neste exemplo, usamos o método `parse` para converter nossa string em um objeto de data e, em seguida, usamos o método `strftime` para formatar a data de acordo com o padrão de dia/mês/ano.

## Profundidade

Além dos métodos mencionados acima, Ruby também tem uma biblioteca de classes de datas mais avançadas, como `Date`, `DateTime` e `Time`. Cada uma dessas classes tem suas próprias vantagens e métodos específicos para calcular datas no futuro ou passado. Se você quiser se aprofundar nesse assunto e aprender mais sobre como essas classes funcionam, recomendo verificar a documentação oficial do Ruby (links na seção "Veja também").

# Veja também

- Documentação oficial do Ruby: https://www.ruby-lang.org/pt/documentation/
- Ruby Date class: https://ruby-doc.org/core-3.0.1/Date.html
- Ruby DateTime class: https://ruby-doc.org/stdlib-3.0.1/libdoc/date/rdoc/DateTime.html
- Ruby Time class: https://ruby-doc.org/core-3.0.1/Time.html