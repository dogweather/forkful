---
title:    "Ruby: Unindo strings"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/ruby/concatenating-strings.md"
---

{{< edit_this_page >}}

## Por que concatenar strings?

Quando estamos programando, muitas vezes precisamos combinar diferentes strings para criar uma mensagem ou manipular dados de maneira mais eficiente. Concatenar strings é o processo de juntar duas ou mais strings em uma única string. Isso pode ser útil para criar menus de opções, gerar relatórios ou até mesmo construir URLs. Neste post, vamos aprender como concatenar strings em Ruby e mergulhar em mais detalhes sobre esse processo.

## Como fazer

Em Ruby, podemos concatenar strings usando o operador de adição `+` ou o método `String#concat`. Vamos ver alguns exemplos de como isso funciona:

```Ruby
# Usando o operador de adição
nome = "João"
sobrenome = "Silva"
nome_completo = nome + " " + sobrenome
puts nome_completo # Output: João Silva

# Usando o método String#concat
mensagem = "Olá"
mensagem.concat(" Mundo!")
puts mensagem # Output: Olá Mundo!
```

Podemos também utilizar a interpolação de strings para concatenar valores de variáveis dentro de uma string. Essa técnica é especialmente útil quando precisamos formatar uma mensagem com dados dinâmicos.

```Ruby
# Interpolação de strings
idade = 25
mensagem = "Eu tenho #{idade} anos."
puts mensagem # Output: Eu tenho 25 anos.
```

É importante lembrar que ao concatenar strings em Ruby, o resultado será sempre uma nova string. As variáveis originais não são alteradas, a menos que atribuamos o resultado da concatenação a elas.

## Mergulho profundo

Ao concatenar strings, é importante estar ciente da performance de seu código. Isso porque cada vez que concatenamos uma string, uma nova string é criada na memória, o que pode ser ineficiente quando se lida com grandes quantidades de dados. Nesses casos, é preferível utilizar o método `String#<<`, que adiciona o conteúdo diretamente à string original, ao invés de criar uma nova. Outra opção é utilizar o método `String#%`, que permite substituir variáveis em uma string de forma mais eficiente que a interpolação.

```Ruby
# Utilizando o método String#<<
frase = "Eu gosto de "
frase << "programar em "
frase << "Ruby."
puts frase # Output: Eu gosto de programar em Ruby.

# Utilizando o método String#%
nome = "Maria"
idade = 35
mensagem = "Olá, meu nome é %s e eu tenho %d anos." % [nome, idade]
puts mensagem # Output: Olá, meu nome é Maria e eu tenho 35 anos.
```

Outro ponto importante é prestar atenção nos tipos de dados ao concatenar strings. Por exemplo, se tentarmos concatenar um número com uma string usando `+`, teremos um erro. Para solucionar isso, podemos primeiro converter o número em uma string utilizando o método `to_s`.

## Veja também

- Documentação da linguagem Ruby sobre strings: [https://ruby-doc.org/core-2.7.1/String.html](https://ruby-doc.org/core-2.7.1/String.html)
- Artigo sobre interpolação de strings no Ruby: [https://www.rubyguides.com/2016/04/ruby-string-interpolation/](https://www.rubyguides.com/2016/04/ruby-string-interpolation/)
- Guia sobre concatenação de strings eficiente em Ruby: [https://blog.appsignal.com/2018/05/15/efficient-string-interpolation-in-ruby.html](https://blog.appsignal.com/2018/05/15/efficient-string-interpolation-in-ruby.html)