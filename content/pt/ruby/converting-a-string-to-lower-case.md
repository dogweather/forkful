---
title:                "Ruby: Convertendo uma string para minúsculas."
simple_title:         "Convertendo uma string para minúsculas."
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/ruby/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Por que?

Converter uma string para letras minúsculas é uma tarefa comum em programação, especialmente quando se trata de processar entrada de usuário. Ao converter uma string para minúsculas, podemos garantir que qualquer entrada seja comparada de forma consistente e facilitar a manipulação de dados.

## Como fazer

Existem várias maneiras de converter uma string para letras minúsculas em Ruby, dependendo da situação e preferência pessoal. Vamos dar uma olhada em algumas das formas mais comuns:

```Ruby
# Usando o método .downcase
string = "Olá, MUNDO!"
puts string.downcase # saída: olá, mundo!

# Usando o método .downcase!
string = "Olá, MUNDO!"
puts string.downcase! # saída: olá, mundo!
puts string # saída: olá, mundo! (string foi alterada permanentemente)

# Usando a função String#downcase
string = "Olá, MUNDO!"
puts string.send(:downcase) # saída: olá, mundo!
```

Como podemos ver, existem três maneiras diferentes de converter uma string para minúsculas em Ruby. Os métodos `.downcase` e `.downcase!` são fornecidos pelo Ruby de forma nativa, enquanto a função `String#downcase` é chamada usando o método `send`, que pode ser útil em situações mais complexas.

## Mergulho profundo

Por trás do funcionamento desses métodos e funções está o fato de que todas as strings em Ruby são objetos. Isso significa que eles possuem métodos e atributos que podem ser chamados para manipulá-los. Ao usar o método `.downcase`, estamos essencialmente chamando o método `downcase` no objeto string e obtendo o resultado de volta.

Além disso, é importante notar que o Ruby é uma linguagem sensível a maiúsculas e minúsculas. Isso significa que o resultado da conversão para minúsculas dependerá do idioma usado no seu sistema operacional ou da versão do Ruby sendo executada.

## Veja também

- [Documentação do Ruby: String#downcase](https://ruby-doc.org/core-2.7.1/String.html#method-i-downcase)
- [Livro de Receitas do Ruby: Métodos de string do Ruby](https://www.rubyguides.com/2019/05/ruby-string-methods/) 
- [Convertendo strings para minúsculas em Ruby](https://medium.com/swlh/convert-strings-to-upcase-or-downcase-with-ruby-fac109971f)