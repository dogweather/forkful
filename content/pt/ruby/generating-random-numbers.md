---
title:    "Ruby: Gerando números aleatórios"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/ruby/generating-random-numbers.md"
---

{{< edit_this_page >}}

##Por que?

Gerar números aleatórios é uma habilidade fundamental em programação, especialmente em jogos ou em tarefas que exigem aleatoriedade. Ao entender como gerar números aleatórios, você pode adicionar um elemento imprevisível aos seus programas e torná-los mais dinâmicos e divertidos.

##Como fazer?

Felizmente, a linguagem Ruby possui uma função embutida para gerar números aleatórios - o método `rand`. Este método pode ser usado de diferentes maneiras, dependendo do tipo de número aleatório que você deseja gerar.

###Números Inteiros

Para gerar um número inteiro aleatório entre dois números específicos, podemos usar o método `rand(x..y)`, onde "x" é o menor número e "y" é o maior número. Por exemplo, se quisermos gerar um número aleatório entre 1 e 10, podemos usar o seguinte código:

```Ruby
num = rand(1..10)
puts num # Exemplo de output: 7
```

###Números Decimais

Se quisermos gerar um número decimal aleatório entre 0 e 1, podemos usar o método `rand`. No entanto, este método não aceita parâmetros e simplesmente gera um número aleatório com muitos dígitos após a vírgula. Para limitar o número de dígitos, podemos usar o método `round` para arredondar o número para o número de casas decimais desejado. Aqui está um exemplo:

```Ruby
num = rand.round(2)
puts num # Exemplo de output: 0.79
```

###Números Aleatórios de uma determinada lista

Além de gerar números aleatórios dentro de um intervalo específico, podemos também gerar números aleatórios de uma determinada lista. Para fazer isso, podemos criar uma matriz com os números que queremos e usar o método `sample` para selecionar um número aleatório dessa lista. Aqui está um exemplo:

```Ruby
numeros = [3, 8, 12, 19, 25]
num = numeros.sample
puts num # Exemplo de output: 12
```

##Aprofundando-se

Agora que já sabemos como gerar números aleatórios em Ruby, é importante entender como funciona o processo por trás desses números. O método `rand` usa um algoritmo chamado de "Gerador de Números Aleatórios Pseudoaleatórios" (PRNG, na sigla em inglês). Este algoritmo usa uma "semente" (um número inicial) e gera uma sequência de números aparentemente aleatórios, mas que podem ser reproduzidos se a mesma semente for usada.

A semente padrão do método `rand` é baseada no tempo atual, o que significa que cada vez que o programa é executado, uma semente diferente é usada e uma sequência diferente de números é gerada. No entanto, também podemos especificar uma semente personalizada, o que permitirá que a mesma sequência de números aleatórios seja gerada sempre que o programa for executado.

##Veja também

- [Tutorial: Ruby for Absolute Beginners (em português)](https://www.ruby-lang.org/pt/documentation/quickstart/)
- [Documentação oficial do método `rand`](https://ruby-doc.org/core-3.0.1/Kernel.html#method-i-rand)
- [Artigo: Entendendo a geração de números aleatórios em Ruby (em inglês)](https://www.rubyguides.com/2020/05/pseudo-random-number-ruby/)