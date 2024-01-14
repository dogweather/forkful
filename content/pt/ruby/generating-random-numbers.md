---
title:                "Ruby: Gerando números aleatórios"
programming_language: "Ruby"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/ruby/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Por que

Gerar números aleatórios é uma parte importante da programação em Ruby. Pode ser útil em diversas situações, como jogos, sorteios e até mesmo na criação de testes. A capacidade de gerar números aleatórios é uma habilidade valiosa para qualquer programador.

## Como fazer

Em Ruby, a geração de números aleatórios é possível através do método `rand()`. Este método pode receber dois parâmetros, o primeiro sendo o limite superior do intervalo e o segundo sendo o número de repetições. Por exemplo, para gerar um número aleatório entre 1 e 10:

```Ruby
random_number = rand(1..10)
puts random_number
# Exemplo de saída: 7
```

Caso deseje gerar mais de um número aleatório, basta especificar o número de repetições. Por exemplo, para gerar 5 números aleatórios entre 1 e 100:

```Ruby
5.times do
    random_number = rand(1..100)
    puts random_number
end
# Exemplo de saída:
# 53
# 94
# 16
# 27
# 81
```

Além disso, se você quiser gerar números aleatórios com decimais, basta utilizar o método `rand()` sem parâmetros ou especificar o número de casas decimais desejado. Por exemplo:

```Ruby
random_number = rand()
puts random_number
# Exemplo de saída: 0.7257914410117215

# Ou, sendo mais específico:
random_number = rand.round(2)
puts random_number
# Exemplo de saída: 0.34
```

## Deep Dive

Por trás do método `rand()` há um algoritmo complexo que é responsável pela geração de números aleatórios. Ele se baseia em um número de semente (seed) que é utilizado para calcular o próximo número aleatório. Em Ruby, a semente padrão é o tempo atual em segundos desde 1970. Isso significa que, se você executar o mesmo código em um intervalo de tempo muito curto, os mesmos números aleatórios serão gerados.

É importante ressaltar que o método `rand()` gera números pseudoaleatórios, ou seja, eles parecem aleatórios, mas na verdade são calculados por um algoritmo. Isso pode ser um problema caso você precise de números verdadeiramente aleatórios para criptografia, por exemplo. Nesse caso, é recomendado utilizar uma biblioteca externa que possua algoritmos de geração de números criptograficamente seguros.

## Veja também

- [Documentação do método `rand()`](https://ruby-doc.org/core-2.7.2/Kernel.html#method-i-rand)
- [Api da gem SecureRandom](https://ruby-doc.org/stdlib-2.7.2/libdoc/securerandom/rdoc/SecureRandom.html)
- [Artigo sobre geração de números aleatórios em Ruby](https://www.rubyguides.com/2018/08/ruby-random/)