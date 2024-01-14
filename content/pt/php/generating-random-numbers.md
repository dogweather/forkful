---
title:                "PHP: Geração de números aleatórios"
simple_title:         "Geração de números aleatórios"
programming_language: "PHP"
category:             "PHP"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/php/generating-random-numbers.md"
---

{{< edit_this_page >}}

# Por que gerar números aleatórios em programação PHP?

Muitas vezes, em programação PHP, é necessário gerar números aleatórios para diversas finalidades. Seja para criar números de identificação ou para realizar sorteios, a geração de números aleatórios é uma funcionalidade importante que pode ser facilmente implementada em suas aplicações.

## Como gerar números aleatórios em PHP

Para gerar números aleatórios em PHP, utilizamos a função `rand()` juntamente com um intervalo de valores. Por exemplo, se quisermos gerar um número aleatório entre 1 e 10, utilizamos o seguinte código:

```PHP
$randomNumber = rand(1, 10);
echo "Número aleatório gerado: " . $randomNumber;
```

A saída deste código pode ser, por exemplo, "Número aleatório gerado: 7". Podemos criar códigos mais complexos, como gerar uma senha aleatória com uma combinação de letras e números:

```PHP
$alphabet = 'abcdefghijklmnopqrstuvwxyz';
$randomPassword = substr(str_shuffle($alphabet), 0, rand(6, 12));
echo "Senha aleatória gerada: " . $randomPassword;
```

A saída pode ser, por exemplo, "Senha aleatória gerada: c58op9". A combinação desses dois exemplos pode ser utilizada para criar diferentes tipos de códigos que envolvam números aleatórios.

## Aprofundando-se na geração de números aleatórios em PHP

Ao utilizar a função `rand()`, é importante lembrar que ela depende de um gerador de números aleatórios (RNG) que é definido pelo sistema operacional ou ambiente de hospedagem. Em alguns casos, pode ocorrer de números não tão aleatórios serem gerados, o que pode comprometer a segurança de suas aplicações. Para garantir mais segurança, é possível utilizar a função `mt_rand()` que utiliza um gerador de números aleatórios melhorado.

Também é importante mencionar que a função `rand()` e `mt_rand()` geram números pseudoaleatórios, ou seja, não são realmente aleatórios, mas sim uma sequência de números que parecem ser aleatórios. Para criar números verdadeiramente aleatórios, é necessário utilizar uma fonte externa de entropia, como arquivos ou dispositivos que fornecem valores aleatórios.

# Veja também

- [Documentação da função `rand()` em PHP](https://www.php.net/manual/pt_BR/function.rand.php)
- [Documentação da função `mt_rand()` em PHP](https://www.php.net/manual/pt_BR/function.mt-rand.php)
- [Artigo sobre segurança na geração de números aleatórios em PHP](https://www.php-fusion.co.uk/infusions/articles/documentation.php?cat_id=24&article_id=29)