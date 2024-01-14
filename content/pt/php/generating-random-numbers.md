---
title:    "PHP: Gerando números aleatórios"
keywords: ["PHP"]
---

{{< edit_this_page >}}

## Por que gerar números aleatórios?

Gerar números aleatórios é uma tarefa comum em muitos projetos de programação. Isso pode ser útil para simulações, jogos, criptografia e muitos outros fins. Além disso, pode ser uma forma divertida de aprender novas habilidades de programação.

## Como fazer isso em PHP?

Gerar números aleatórios em PHP é bastante simples. Basta usar a função `rand()` para gerar um número aleatório entre um intervalo especificado. Por exemplo:

```PHP
// gera um número aleatório entre 1 e 10
$numero = rand(1, 10);
echo $numero;
```

Você também pode utilizar a função `mt_rand()` para gerar um número aleatório baseado no algoritmo Mersenne Twister, que é considerado mais eficiente. Veja:

```PHP
// gera um número aleatório entre 100 e 500
$numero = mt_rand(100, 500);
echo $numero;
```

Além disso, você pode usar a função `mt_srand()` para definir uma semente para seu número aleatório, o que pode ser útil para obter resultados consistentes em diferentes execuções do código.

## Profundidade na geração de números aleatórios

Embora a função `rand()` seja simples e eficiente, ela não é considerada criptograficamente segura. Isso significa que os números gerados não devem ser usados em contextos onde a segurança é importante. Para esses casos, é recomendado usar a função `random_int()`, que usa um gerador de números aleatórios criptograficamente seguro.

Além disso, é possível especificar a precisão dos números gerados pela função `mt_rand()`, utilizando as funções `mt_getrandmax()` e `mt_rand()`. Isso pode ser útil em casos específicos, como em jogos de cassino, por exemplo.

## Veja também

- [Documentação oficial do PHP sobre funções de geração de números aleatórios](https://www.php.net/manual/pt_BR/ref.math.php)
- [Tutorial da W3Schools sobre geração de números aleatórios em PHP](https://www.w3schools.com/php/func_math_rand.asp)
- [Random.org - Um gerador de números aleatórios que utiliza fontes verdadeiramente aleatórias](https://www.random.org/)