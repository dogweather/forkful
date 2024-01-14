---
title:                "Ruby: Encontrando o comprimento de uma string"
programming_language: "Ruby"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/ruby/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Por que
Se você é engajado em aprender linguagens de programação ou já é um desenvolvedor, provavelmente já se deparou com a necessidade de encontrar o comprimento de uma string (cadeia de caracteres). Embora possa parecer uma tarefa simples, entender como encontrar o comprimento de uma string é uma habilidade fundamental para trabalhar com dados de texto em Ruby.

## Como Fazer
Encontrar o comprimento de uma string é uma tarefa fácil em Ruby. Basta usar o método #length ou #size da classe String. Por exemplo:
```Ruby
nome = "João"
puts nome.length
puts nome.size
```
Neste exemplo, usamos o método #length e #size para encontrar o comprimento da string "João". O output será `4` em ambos os casos, pois a string contém quatro caracteres.

Você também pode usar o método #length ou #size diretamente em uma string sem atribuí-la a uma variável. Por exemplo:
```Ruby
puts "Olá".length
puts "Olá".size
```
O output será novamente `4`.

## Profundidade
Ao encontrar o comprimento de uma string em Ruby, é importante entender que ele conta todos os caracteres, incluindo espaços e caracteres especiais. Por exemplo, se tivermos a string "olá mundo!" e contarmos o comprimento usando o método #length ou #size, o output será `11`, pois há onze caracteres, incluindo o espaço entre as palavras.

Outro detalhe importante é que o método #length e #size retornam um número inteiro, o que significa que não são adequados para contar a quantidade de palavras em uma string. Para isso, pode-se usar o método #split combinado com o método #count ou #length. Por exemplo:
```Ruby
frase = "Esta é uma frase de exemplo"
palavras = frase.split(" ")
puts palavras.count 
# ou puts palavras.length
```
O output será `6` neste caso, pois a string foi dividida em seis palavras.

## Veja Também
- [Documentação do método #length em Ruby](https://ruby-doc.org/core-2.7.1/String.html#method-i-length)
- [Documentação do método #size em Ruby](https://ruby-doc.org/core-2.7.1/String.html#method-i-size)
- [Como contar palavras em Ruby](https://www.rubyguides.com/2018/11/ruby-count-words/)

O comprimento de uma string é um conceito simples, mas é uma ferramenta poderosa ao trabalhar com dados em texto em Ruby. Com o conhecimento deste conceito e dos métodos corretos, você pode manipular e analisar facilmente strings em seus projetos. Espero que este artigo tenha sido útil para você em seu aprendizado de Ruby. Até a próxima!