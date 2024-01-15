---
title:                "Imprimindo saídas de depuração"
html_title:           "Ruby: Imprimindo saídas de depuração"
simple_title:         "Imprimindo saídas de depuração"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/ruby/printing-debug-output.md"
---

{{< edit_this_page >}}

## Por que

Você já se viu preso em um código que não está funcionando corretamente e não consegue descobrir o problema? Ou talvez você apenas queira entender melhor o fluxo de execução do seu programa? Em ambos os casos, imprimir saída de depuração pode ser uma ferramenta extremamente útil.

## Como fazer

Para imprimir saída de depuração em Ruby, usamos o método `puts`. Veja um exemplo:

```Ruby
x = 5
y = 3

puts "O valor de x é #{x}"
puts "O valor de y é #{y}"
```

A saída do código acima seria:

```
O valor de x é 5
O valor de y é 3
```

Podemos também combinar o método `puts` com `inspect` para imprimir o valor e o tipo de uma variável:

```Ruby
x = "Olá mundo"

puts "O valor de x é #{x.inspect}"
```

A saída seria:

```
O valor de x é "Olá mundo"
```

## Mergulho profundo

Apesar de simples, imprimir saída de depuração pode ser uma técnica poderosa para entender o funcionamento do seu código. Alguns truques úteis incluem adicionar mensagens de depuração em pontos estratégicos do seu código, como no início e no final de um loop ou função, e usar `inspect` para visualizar objetos complexos.

Além disso, também é possível utilizar o método `p`, que é semelhante ao `puts` e `inspect` combinados. Ele imprime diretamente o valor e o tipo da variável sem precisar do método `inspect`.

Esperamos que esse artigo te ajude a utilizar saída de depuração de forma efetiva em seus projetos!

## Veja também

- [Documentação oficial do Ruby sobre debug e logging](https://ruby-doc.org/core-3.0.0/doc/debug.html)
- [Artigo da Ruby Guides sobre saída de depuração](https://www.rubyguides.com/2019/01/debugging-in-ruby/)