---
title:    "PHP: Obtendo a data atual."
keywords: ["PHP"]
---

{{< edit_this_page >}}

## Por que obter a data atual?

Obter a data atual é uma tarefa muito comum em programação, especialmente em PHP. A data é uma informação importante em muitos casos, como no registro de eventos, criação de arquivos ou na construção de relatórios. Além disso, a data é uma informação dinâmica, que muda constantemente, e é essencial para manter dados atualizados e precisos.

## Como obter a data atual em PHP?

Existem várias maneiras de obter a data atual em PHP, mas uma das mais simples é usando a função `date()`. Vamos ver como isso funciona em um exemplo prático:

```
<?php
$currentDate = date('d/m/Y');
echo "A data atual é: " . $currentDate;
```

O código acima irá imprimir a data atual no formato de dia/mês/ano, como por exemplo: "12/06/2020". O primeiro argumento da função `date()` determina o formato da data que será retornada. Você pode encontrar uma lista completa de formatos de data no [manual do PHP](https://www.php.net/manual/pt_BR/function.date.php).

## Mergulhando mais fundo

Além de retornar a data atual, a função `date()` também permite que você adicione ou subtraia dias, semanas, meses ou anos da data atual. Por exemplo:

```
<?php
$futureDate = date('d/m/Y', strtotime('+1 week'));
echo "A data atual + 1 semana é: " . $futureDate;
```

Neste caso, a data resultante seria "19/06/2020". Você também pode criar datas personalizadas definindo o segundo argumento da função `date()` como um timestamp. Isso pode ser útil para gerar datas específicas para testes ou simular informações históricas.

## Veja também

- [Manual do PHP: Função date()](https://www.php.net/manual/pt_BR/function.date.php)
- [Manual do PHP: Formatação de data](https://www.php.net/manual/pt_BR/datetime.format.php)
- [Tutorial do W3Schools: Manipulando datas em PHP](https://www.w3schools.com/php/php_date.asp)