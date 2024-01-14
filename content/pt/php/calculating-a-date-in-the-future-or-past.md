---
title:                "PHP: Calculando uma data no futuro ou no passado"
programming_language: "PHP"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/php/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

##Por que

Às vezes, durante o desenvolvimento de um site ou aplicativo, pode ser necessário calcular uma data no futuro ou no passado com base em um determinado valor ou evento. Isso é comum em tarefas como agendamento de compromissos, alertas de renovação e muito mais.

##Como Fazer

Para realizar esse cálculo em PHP, é necessário utilizar a função `strtotime()` combinada com o operador `+` ou `-`. Por exemplo, para obter a data de hoje mais 10 dias, podemos fazer o seguinte código:

```PHP
$data_futura = strtotime("+10 days");
echo date('d/m/Y', $data_futura);
```

Isso resultará na saída `30/06/2020`, considerando que a data de hoje é `20/06/2020`.

Da mesma forma, para obter uma data no passado, podemos usar o operador `-`. No exemplo abaixo, obteremos a data de três semanas atrás:

```PHP
$data_passada = strtotime("-3 weeks");
echo date('d/m/Y', $data_passada);
```

Isso resultará na saída `30/05/2020`.

##Mergulho Profundo

A função `strtotime()` aceita uma variedade de parâmetros, permitindo que você defina a data inicial e o valor a ser adicionado ou subtraído em vários formatos. Alguns exemplos são:

- `strtotime("+2 months")` para adicionar 2 meses
- `strtotime("+1 year +2 weeks")` para adicionar 1 ano e 2 semanas
- `strtotime("next Monday")` para obter a próxima segunda-feira

Você também pode definir a partir de uma data específica usando a função `mktime()`. Por exemplo, para obter a data de 10 dias após 20 de maio de 2020, podemos fazer o seguinte código:

```PHP
$data_inicio = mktime(0, 0, 0, 5, 20, 2020);
$data_futura = strtotime("+10 days", $data_inicio);
echo date('d/m/Y', $data_futura);
```

Isso resultará na saída `30/05/2020`.

##Veja Também

- [Documentação oficial sobre strtotime()](https://www.php.net/manual/en/function.strtotime.php)
- [Tutorial: Como trabalhar com datas em PHP](https://www.phpk.org/php-time-date-output-format-func-php/)