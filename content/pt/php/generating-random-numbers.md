---
title:                "Geração de números aleatórios"
html_title:           "PHP: Geração de números aleatórios"
simple_title:         "Geração de números aleatórios"
programming_language: "PHP"
category:             "PHP"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/php/generating-random-numbers.md"
---

{{< edit_this_page >}}

# O que e por que?

Gerar números aleatórios é uma técnica muito utilizada na programação para produzir resultados imprevisíveis. Isso é especialmente útil em jogos e aplicações de segurança, onde a repetibilidade de um resultado pode comprometer a integridade do sistema.

# Como fazer:

Para gerar um número aleatório em PHP, podemos usar a função ```rand()```, que recebe dois parâmetros opcionais: o valor mínimo e máximo do intervalo desejado. Veja o exemplo abaixo:

```
<?php 
$num_aleatorio = rand(1, 10); // gera um número entre 1 e 10
echo $num_aleatorio;
```

A saída deste código pode ser, por exemplo, o número 7.

# Mergulho profundo:

Historicamente, gerar números aleatórios era uma tarefa complexa e muitas vezes imprecisa. Antigamente, era comum utilizar métodos baseados em eventos aleatórios do mundo real, como lançar dados ou baralhos de cartas. Com o avanço da tecnologia, foram desenvolvidos algoritmos matemáticos mais eficientes para gerar números aleatórios.

Além da função ```rand()```, PHP também possui a função ```mt_rand()```, que usa um algoritmo mais seguro e é recomendada para aplicações que requerem um alto nível de aleatoriedade.

Uma alternativa ao uso das funções nativas do PHP para gerar números aleatórios é o uso de serviços externos, como APIs que fornecem números aleatórios criptografados e seguros.

# Veja também:

- Documentação oficial do PHP sobre a função ```rand()```: https://www.php.net/manual/pt_BR/function.rand.php
- Documentação oficial do PHP sobre a função ```mt_rand()```: https://www.php.net/manual/pt_BR/function.mt-rand.php