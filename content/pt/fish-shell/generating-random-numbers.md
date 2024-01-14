---
title:                "Fish Shell: Gerando números aleatórios"
simple_title:         "Gerando números aleatórios"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/fish-shell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Por que
Algumas vezes, durante o desenvolvimento de um programa, podemos precisar gerar números aleatórios para simular dados ou para testar alguma funcionalidade específica. O uso de números aleatórios pode trazer mais diversidade e realismo aos nossos testes e também podem ser úteis em jogos e sorteios. A geração de números aleatórios é uma habilidade importante para programadores e a linguagem Fish Shell oferece uma maneira simples de incorporar essa funcionalidade em nossos scripts.

## Como Fazer
Para gerar números aleatórios em uma instância da Fish Shell, podemos utilizar o comando `math random` e especificar o intervalo entre os números desejados. Por exemplo, para gerar um número aleatório entre 1 e 10, podemos usar o seguinte código:

```Fish Shell
set numero_aleatorio (math random 1 10)
echo $numero_aleatorio
```

Ao executar esse código, o terminal irá exibir um número aleatório entre 1 e 10, que pode ser diferente a cada vez que o script é executado. Podemos também usar variáveis no lugar dos números específicos:

```Fish Shell
set minimo 50
set maximo 100
set numero_aleatorio (math random $minimo $maximo)
echo $numero_aleatorio
```

Nessa situação, o número aleatório será gerado dentro do intervalo entre as variáveis minimo e maximo. Podemos até mesmo gerar números decimais, especificando um terceiro parâmetro com a quantidade de casas decimais desejadas:

```Fish Shell
set numero_decimal (math random 0 1 3)
echo $numero_decimal
```

O comando `math random` também possui outras opções, como a possibilidade de gerar mais de um número aleatório por vez e a geração de números a partir de uma semente específica.

## Mergulho Profundo
Por trás dos bastidores, o comando `math random` utiliza o gerador de números pseudoaleatórios do C, que é um algoritmo matemático que produz uma sequência de números que parece ser aleatória, mas na verdade é determinística. Essa sequência é determinada por uma semente, que pode ser especificada ou gerada automaticamente pelo sistema. 

É importante lembrar que os números gerados dessa maneira não são totalmente aleatórios e não devem ser utilizados para fins criptográficos. Para isso, precisamos de um gerador de números verdadeiramente aleatórios, como um gerador de números quânticos.

## Veja Também
- [Documentação do comando `math random`](https://fishshell.com/docs/current/commands.html#math)
- [Gerador de Números Aleatórios Pseudoaleatórios](https://en.wikipedia.org/wiki/Pseudorandom_number_generator)
- [Gerador de Números Quânticos](https://pt.wikipedia.org/wiki/Amplificador_de_vácuo)