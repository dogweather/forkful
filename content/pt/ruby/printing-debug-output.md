---
title:                "Ruby: Imprimindo saída de depuração"
simple_title:         "Imprimindo saída de depuração"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/ruby/printing-debug-output.md"
---

{{< edit_this_page >}}

## Por que

Às vezes, enquanto escrevemos um código em Ruby, podemos nos deparar com erros ou problemas que não conseguimos resolver imediatamente. Nestes casos, pode ser útil imprimir saídas de depuração para entender melhor o que está acontecendo em nosso código.

## Como Fazer

Podemos imprimir saídas de depuração usando o método `puts` ou `p` em Ruby. Por exemplo, se quisermos verificar os valores de uma variável `x` em nosso código, podemos usar `puts x` ou `p x` para imprimir o valor atual dessa variável no console. Vamos ver um exemplo prático:

```Ruby
x = 10
puts x # output: 10
p x # output: 10
```

Dessa forma, podemos ver facilmente o valor atual de `x` e usar essas saídas de depuração para identificar e corrigir possíveis erros ou problemas em nosso código.

## Mergulho Profundo

Além de apenas imprimir os valores de variáveis, também podemos usar saídas de depuração para entender melhor o fluxo de nosso código. Por exemplo, podemos imprimir mensagens antes e depois de um determinado trecho de código para verificar se está sendo executado corretamente ou não.

Outra técnica útil é adicionar um número de linha nos nossos `puts` ou `p` para ajudar a identificar de onde está vindo a saída de depuração. Por exemplo:

```Ruby
x = 10
puts "Valor de x na linha 1: " + x # output: Valor de x na linha 1: 10
p "Valor de x na linha 2: " + x # output: Valor de x na linha 2: 10
```

Isso nos permite saber em qual parte específica de nosso código a saída de depuração está sendo gerada e facilita a localização do problema.

## Veja Também

- [Debugging in Ruby](https://guides.rubyonrails.org/debugging_rails_applications.html)
- [Using puts and p to Debug Ruby Code](https://www.rubyguides.com/2019/07/ruby-puts-vs-p/)
- [Debugging Ruby with Pry](https://pryhq.com/)