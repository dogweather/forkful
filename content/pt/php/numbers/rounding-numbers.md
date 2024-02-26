---
date: 2024-01-26 03:46:07.410917-07:00
description: "Arredondar n\xFAmeros significa cortar os decimais at\xE9 uma precis\xE3\
  o definida, frequentemente para n\xFAmeros inteiros. Programadores arredondam para\
  \ simplificar\u2026"
lastmod: '2024-02-25T18:49:44.282830-07:00'
model: gpt-4-0125-preview
summary: "Arredondar n\xFAmeros significa cortar os decimais at\xE9 uma precis\xE3\
  o definida, frequentemente para n\xFAmeros inteiros. Programadores arredondam para\
  \ simplificar\u2026"
title: "Arredondamento de n\xFAmeros"
---

{{< edit_this_page >}}

## O Que & Por Que?
Arredondar números significa cortar os decimais até uma precisão definida, frequentemente para números inteiros. Programadores arredondam para simplificar cálculos, melhorar o desempenho ou tornar as saídas amigáveis ao usuário.

## Como fazer:
O PHP oferece algumas maneiras de arredondar números: `round()`, `ceil()` e `floor()`. Veja como eles funcionam:

```php
echo round(3.14159);   // Retorna 3
echo round(3.14159, 2); // Retorna 3.14

echo ceil(3.14159);    // Retorna 4, sempre arredonda para cima

echo floor(3.14159);   // Retorna 3, sempre arredonda para baixo
```

## Aprofundamento
Arredondar números é essencial em matemática e computação desde os tempos antigos para lidar com decimais infinitos impraticáveis. No PHP, `round()` pode receber um parâmetro de precisão e modo, afetando seu comportamento - `PHP_ROUND_HALF_UP`, `PHP_ROUND_HALF_DOWN`, etc., definem como ele se comportará ao encontrar um cenário ".5". A precisão é chave em aplicações financeiras, nas quais o arredondamento pode ser legalmente regulado, afetando como `round()` é implementado no código.

Alternativas às funções integradas incluem métodos de arredondamento personalizados ou funções de Matemática BC para aritmética de precisão arbitrária, que são úteis para cenários que necessitam mais controle ou lidam com números muito grandes onde a precisão nativa pode falhar.

## Veja Também
Explore mais no manual do PHP:
- [Função `round` do PHP](https://www.php.net/manual/en/function.round.php)
- [Função `ceil` do PHP](https://www.php.net/manual/en/function.ceil.php)
- [Função `floor` do PHP](https://www.php.net/manual/en/function.floor.php)
- [Matemática BC para aritmética de precisão arbitrária](https://www.php.net/manual/en/book.bc.php)
