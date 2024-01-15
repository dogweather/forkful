---
title:                "Gerando números aleatórios"
html_title:           "PHP: Gerando números aleatórios"
simple_title:         "Gerando números aleatórios"
programming_language: "PHP"
category:             "PHP"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/php/generating-random-numbers.md"
---

{{< edit_this_page >}}

##Por que

Se você está pensando em criar um jogo, testar algoritmos ou até mesmo garantir apoio em decisões aleatórias, gerar números aleatórios pode ser uma ferramenta útil e divertida. A linguagem de programação PHP oferece uma maneira simples e eficaz de gerar números aleatórios, que podem ser usados para diversas finalidades.

##Como Fazer

Gerar números aleatórios em PHP é simples e pode ser feito utilizando a função "rand" que retorna um número inteiro aleatório entre dois valores pré-definidos. Abaixo, você pode ver um exemplo de como usar a função "rand" para gerar um número entre 1 e 10 e imprimi-lo na tela:

```PHP
$num = rand(1, 10);
echo $num; //saída: número aleatório entre 1 e 10
```

Além disso, você também pode utilizar a função "mt_rand" que gera números aleatórios utilizando o gerador Mersenne Twister. Essa função oferece uma gama maior de números aleatórios e é recomendada para uso em aplicações críticas ou altamente sensíveis. Veja um exemplo de como usar a função "mt_rand" abaixo:

```PHP
$num = mt_rand(100, 1000);
echo $num; //saída: número aleatório entre 100 e 1000
```

Você também pode gerar números aleatórios com ponto flutuante utilizando a função "mt_rand" em conjunto com a função "floatval". No exemplo abaixo, vamos gerar um número aleatório com até duas casas decimais:

```PHP
$num = mt_rand(1, 100);
$num = floatval($num) / 100;
echo $num; //saída: número aleatório com duas casas decimais entre 0.01 e 1.00
```

Agora que você aprendeu como gerar números aleatórios em PHP, vamos dar uma olhada mais aprofundada nessa função e como ela funciona.

##Deep Dive

As funções "rand" e "mt_rand" funcionam através da geração de números pseudoaleatórios. Isso significa que, apesar de parecerem aleatórios, esses números são calculados a partir de uma fórmula matemática e, portanto, são previsíveis. No entanto, a cada vez que a função é executada, a semente (seed) utilizada no cálculo é alterada, garantindo assim diferentes resultados.

Para garantir uma maior variação entre os números gerados, é possível definir a semente manualmente, passando um valor como parâmetro para a função. Isso geralmente é feito utilizando o tempo atual em milissegundos ou o número PID do processo atual. Veja um exemplo de como definir a semente manualmente:

```PHP
mt_srand(time());
$num = mt_rand(1, 1000);
echo $num; //saída: número aleatório entre 1 e 1000 usando o tempo atual como semente
```

Você também pode usar a função "shuffle" para embaralhar um array em ordem aleatória. Veja um exemplo abaixo:

```PHP
$array = array(1, 2, 3, 4, 5);
shuffle($array);
print_r($array); //saída: array em ordem aleatória
```

Além disso, o PHP também possui uma função chamada "array_rand" que retorna uma ou mais chaves aleatórias de um array. Veja um exemplo de como usá-la:

```PHP
$array = array("a" => "maçã", "b" => "banana", "c" => "laranja", "d" => "morango");
$key = array_rand($array);
echo $key; //saída: uma chave aleatória do array, como por exemplo "b"
echo $array[$key]; //saída: o elemento correspondente à chave escolhida, neste caso "banana"
```

Como mencionado anteriormente, a função "mt_rand" é recomendada para uso em aplicações críticas ou altamente sensíveis. Para garantir uma maior segurança na geração de números aleatórios, o PHP também possui uma extensão chamada "openssl" que oferece uma função chamada "openssl_random_pseudo_bytes". Essa função utiliza o gerador de números aleatórios do sistema operacional, garantindo assim uma maior aleatoriedade nos números gerados. Veja um exemplo de como usar a função