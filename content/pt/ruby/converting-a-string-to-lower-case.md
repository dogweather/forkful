---
title:    "Ruby: Convertendo uma string para minúsculas."
keywords: ["Ruby"]
---

{{< edit_this_page >}}

## Por que

Ao trabalhar com strings em Ruby, muitas vezes é necessário alterar o caso das letras para satisfazer determinadas condições ou formatos. Converter uma string para letras minúsculas, por exemplo, é uma tarefa comum e importante na programação em Ruby.

## Como Fazer

Para converter uma string em letras minúsculas em Ruby, é possível utilizar o método `downcase`. Vejamos um exemplo abaixo:

```Ruby
string = "Olá, MUNDO!"
puts string.downcase
```

Este código irá imprimir "olá, mundo!" na tela. Como podemos ver, ao usar o método `downcase` na variável `string`, todas as letras maiúsculas foram transformadas em minúsculas.

## Aprofundando

Além do método `downcase`, existem outras formas de converter uma string para letras minúsculas em Ruby. Uma delas é o método `swapcase`, que alterna o caso de todas as letras da string. Por exemplo:

```Ruby
string = "Oi mundo!"
puts string.swapcase
```

A saída deste código será "oI MUNDO!". Como podemos ver, todas as letras maiúsculas foram transformadas em minúsculas e vice-versa.

Outro método interessante é o `capitalize`, que converte apenas a primeira letra da string em maiúscula. Por exemplo:

```Ruby
string = "olá, mundo!"
puts string.capitalize
```

A saída será "Olá, mundo!". Como podemos ver, apenas a primeira letra da string foi transformada em maiúscula.

É importante lembrar que estes métodos não alteram a string original, apenas retornam uma nova string com o caso alterado. Para alterar a string original, é preciso atribuir o valor retornado pelo método a variável original. Por exemplo:

```Ruby
string = "Olá, mundo!"
string.downcase!
puts string
```

Agora, a string original foi modificada para "olá, mundo!".

## Veja Também

- [Documentação da linguagem Ruby](https://ruby-doc.org/core-2.7.4/String.html#method-i-downcase)
- [Tutorial de Ruby para Iniciantes](https://www.tutorialspoint.com/ruby/index.htm)
- [Conversão de strings em Ruby](https://www.geeksforgeeks.org/conversion-functions-in-ruby/)

Espero que este artigo tenha sido útil para você compreender melhor como converter strings para letras minúsculas em Ruby. Continue praticando e explorando a linguagem para dominar cada vez mais suas habilidades de desenvolvimento. Até a próxima!