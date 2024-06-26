---
date: 2024-01-26 01:11:28.444969-07:00
description: "Como fazer: Imagine que temos um c\xF3digo repetitivo para cumprimentar\
  \ usu\xE1rios. Em vez disso, vamos envolv\xEA-lo em uma fun\xE7\xE3o chamada `greet_user`."
lastmod: '2024-03-13T22:44:46.672902-06:00'
model: gpt-4-1106-preview
summary: "Imagine que temos um c\xF3digo repetitivo para cumprimentar usu\xE1rios."
title: "Organizando o c\xF3digo em fun\xE7\xF5es"
weight: 18
---

## Como fazer:
Imagine que temos um código repetitivo para cumprimentar usuários. Em vez disso, vamos envolvê-lo em uma função chamada `greet_user`:

```php
function greet_user($name) {
    return "Olá, " . $name . "!";
}

echo greet_user("Alice");
echo greet_user("Bob");
```

Saída:
```
Olá, Alice!
Olá, Bob!
```

Agora, você tem uma ferramenta prática que pode usar a qualquer momento sem reescrever as mesmas linhas de código todas as vezes que quiser dizer olá.

## Aprofundamento
As funções estão na programação desde os primórdios do FORTRAN nos anos 50. Elas são uma pedra angular da programação estruturada e têm a ver com modularidade e isolamento. Alternativas? Bem, você pode adotar a orientação a objetos e falar sobre classes e métodos, que são funções com um traje elegante. Quanto ao PHP, detalhes de implementação incluem especificar valores padrão para parâmetros, indicação de tipo para entradas e a possibilidade de retornar múltiplos valores usando um array ou, a partir do PHP 7.1, uma lista.

Aqui está um toque moderno com declaração de tipo e valores padrão:

```php
function add(float $a, float $b = 0.0): float {
    return $a + $b;
}

echo add(1.5);
echo add(1.5, 2.5);
```

O PHP 7.4 também introduziu funções de seta, ajudando a escrever funções concisas de uma linha só, comumente usadas em operações de array:

```php
$numbers = array(1, 2, 3, 4);
$squared = array_map(fn($n) => $n * $n, $numbers);
print_r($squared);
```

Saída:
```
Array
(
    [0] => 1
    [1] => 4
    [2] => 9
    [3] => 16
)
```

## Veja Também
- [Manual do PHP sobre Funções](https://www.php.net/manual/pt_BR/functions.user-defined.php)
- [PHP: Do Jeito Certo - Funções](https://phptherightway.com/#functions)
- [Aprenda sobre as Funções de Seta do PHP 7.4](https://stitcher.io/blog/short-closures-in-php)
