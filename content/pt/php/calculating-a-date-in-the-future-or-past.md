---
title:    "PHP: Calculando uma data no futuro ou passado"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/php/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Por que

Você já se perguntou como é possível calcular a data no futuro ou no passado usando programação PHP? Bem, esta é uma habilidade útil para aprender a controlar o tempo em seus projetos.

## Como fazer

```PHP
// Calculando a data atual
$dataAtual = date("Y-m-d");

// Calculando a data no futuro com base em um número de dias
$numeroDias = 7;
$dataFutura = date("Y-m-d", strtotime("+$numeroDias days"));

echo "A data daqui a $numeroDias dias é: $dataFutura";
```

A saída deste código seria: "A data daqui a 7 dias é: 2021-04-28". Você também pode calcular a data no passado usando o operador "-".

## Mergulho Profundo

Agora que aprendemos como usar as funções date() e strtotime() para calcular datas no futuro ou no passado, é importante entender seu funcionamento. A função date() retorna a data atual formatada de acordo com o parâmetro especificado, enquanto a função strtotime() analisa a string de data fornecida e a converte em um timestamp.

Com esse conhecimento, podemos manipular facilmente datas em nossos projetos, adicionando ou subtraindo um determinado número de dias, semanas, meses ou anos. Também podemos especificar datas específicas em vez de usar o valor atual com a função strtotime().

## Veja também

- [Documentação oficial do PHP para a função date()](https://www.php.net/manual/en/function.date.php)
- [Documentação oficial do PHP para a função strtotime()](https://www.php.net/manual/en/function.strtotime.php)
- [Tutorial em vídeo: Como calcular datas usando PHP](https://www.youtube.com/watch?v=JbhEh3qJlLU)