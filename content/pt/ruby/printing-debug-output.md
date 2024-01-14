---
title:    "Ruby: Imprimindo saída de depuração"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/ruby/printing-debug-output.md"
---

{{< edit_this_page >}}

## Por que imprimir saída de depuração é importante

Imprimir saída de depuração (debug output) é uma técnica útil para encontrar e corrigir erros em um programa Ruby. Isso permite que os programadores vejam o valor de variáveis e verifiquem se o código está sendo executado corretamente. Além disso, ao imprimir saída de depuração, você pode entender melhor o fluxo do seu código e identificar possíveis problemas.

## Como fazer

Para imprimir saída de depuração em Ruby, podemos usar o método "puts". Vamos dar uma olhada em um exemplo simples:

```Ruby
a = 4
b = 2

puts "O valor de 'a' é #{a}"
puts "O valor de 'b' é #{b}"
```

Neste exemplo, estamos imprimindo a saída de depuração do valor das variáveis "a" e "b". Ao executar esse código, veremos o seguinte resultado:

```
O valor de 'a' é 4
O valor de 'b' é 2
```

Também podemos usar o método "p" para imprimir a saída de depuração. A diferença é que o método "p" exibe o valor da variável e o seu tipo de dados. Vamos adicionar uma linha de código no nosso exemplo anterior:

```Ruby
a = 4
b = 2

puts "O valor de 'a' é #{a}"
puts "O valor de 'b' é #{b}"
p "O tipo de dados de 'a' é #{a.class}"
```

Ao executar esse código, obteremos o seguinte resultado:

```
O valor de 'a' é 4
O valor de 'b' é 2
"O tipo de dados de 'a' é Integer"
```

## Aprofundando-se

Existem várias formas de personalizar e otimizar a saída de depuração no Ruby. Uma delas é usar a instrução "if" para controlar quando a saída de depuração é impressa. Por exemplo, você pode imprimir a saída de depuração apenas quando uma variável for igual a um determinado valor ou maior que um determinado número.

Outra técnica útil é usar o método "inspect" para imprimir o valor de uma variável em um formato mais legível. Por exemplo:

```Ruby
a = "Hello World"
puts a.inspect
```

Esse código irá imprimir a string "Hello World" com aspas, facilitando a leitura e a identificação de possíveis problemas com as aspas ou caracteres especiais.

Por fim, também podemos utilizar ferramentas de depuração integradas ao Ruby, como o "pry", que permite parar a execução do código em um determinado ponto e interagir com as variáveis e o fluxo do programa.

## Veja também

- [Documentação oficial do Ruby sobre saída de depuração](https://ruby-doc.org/core-2.6/Kernel.html#method-i-puts)
- [Artigo sobre saída de depuração no Blog da RubyGuides (em inglês)](https://www.rubyguides.com/2019/03/ruby-debugging/)