---
title:                "PHP: Cálculo de uma data no futuro ou no passado."
simple_title:         "Cálculo de uma data no futuro ou no passado."
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/php/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

##Por que 

O cálculo de datas no passado ou no futuro é uma tarefa comum e útil no desenvolvimento de aplicações de software. Saber como realizar esse cálculo pode ajudar a garantir que sua aplicação exiba as informações corretas no momento certo.

##Como fazer

Você pode usar as funções nativas do PHP para realizar cálculos de datas. Aqui está um exemplo simples de como calcular a data atual mais 7 dias no futuro:

```PHP
$dataAtual = date('Y-m-d');
$futuro = date('Y-m-d', strtotime("+7 days"));
echo "Data atual: ".$dataAtual."<br>";
echo "Data no futuro: ".$futuro;
```

A saída desse código será:

```
Data atual: 2021-03-11
Data no futuro: 2021-03-18
```

Você também pode realizar cálculos com datas passadas, basta usar um número negativo no parâmetro 'days' da função `strtotime`. Outro recurso útil é a função `mktime`, que permite criar uma data específica a partir de valores como ano, mês, dia, etc.

##Aprofundando

O PHP possui diversas funções para manipulação de datas, como por exemplo `strtotime`, `date_add` e `date_diff`. Essas funções permitem não apenas realizar cálculos de datas, mas também formatar e comparar datas.

Você também pode usar o objeto `DateTime`, introduzido na versão 5.2 do PHP, para manipular e calcular datas. Ele possui métodos específicos para adicionar, subtrair e comparar datas.

É importante ter cuidado ao fazer cálculos de datas, levando em consideração fatores como fuso horário e horário de verão. É recomendado sempre usar funções e recursos nativos do PHP para garantir a precisão e evitar erros no cálculo das datas.

##Veja também

- [Documentação do PHP sobre datas](https://www.php.net/manual/pt_BR/datetime.formats.date.php)
- [Tutorial sobre manipulação de datas no PHP](https://www.php.net/manual/pt_BR/datetime.formats.date.php)
- [Utilizando o objeto DateTime no PHP](https://programadorbr.com/manipulacao-de-datas-em-php/)