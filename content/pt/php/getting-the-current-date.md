---
title:                "PHP: Obtendo a data atual"
programming_language: "PHP"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/php/getting-the-current-date.md"
---

{{< edit_this_page >}}

##Por Que

A obtenção da data atual é uma tarefa comum e necessária em muitos projetos de programação. Conhecer a data atual pode ser útil para exibir informações em um formato legível, registrar atividades do usuário ou realizar cálculos de tempo em suas aplicações. Neste artigo, veremos como obter a data atual em PHP e algumas dicas para lidar com datas em seus projetos.

##Como Fazer

Para obter a data atual em PHP, podemos utilizar a função `date()`. Esta função aceita dois parâmetros: o formato da data que desejamos obter e, opcionalmente, o timestamp da data desejada. Vamos dar uma olhada nesses parâmetros em detalhes:

```PHP
$data_atual = date("d/m/Y"); // Retorna a data atual no formato "dia/mês/ano"
echo $data_atual; // Saída: 17/08/2021

$data_especifica = date("Y-m-d", strtotime("01/01/2022")); // Retorna a data especificada em "ano-mês-dia"
echo $data_especifica; // Saída: 2022-01-01
```

Além disso, também é possível obter a data atual do servidor utilizando a função `getdate()`. Esta função retorna um array com informações detalhadas sobre a data atual, como dia, mês, ano, dia da semana, entre outros. Vamos ver um exemplo:

```PHP
$data_atual = getdate();
echo "Hoje é ".$data_atual['mday']."/".$data_atual['mon']."/".$data_atual['year']; // Saída: Hoje é 17/08/2021
```

##Mergulho Profundo

Ao lidar com datas em seus projetos, é importante ter atenção a alguns pontos. Primeiramente, é necessário ter certeza de que o fuso horário do servidor está corretamente configurado. Além disso, é sempre recomendado armazenar datas em formatos universais, como o timestamp, para facilitar conversões e cálculos de tempo.

O PHP também possui funções para manipulação de datas, como `strtotime()` e `date_create()`, que podem ser úteis em determinadas situações. É importante conhecer essas funções e suas variações para utilizar as melhores abordagens em seus projetos.

##Veja Também

- [Documentação do PHP sobre a função `date()`](https://www.php.net/manual/en/function.date.php)
- [Guia completo sobre manipulação de datas em PHP](https://www.php.net/manual/en/datetime.formats.php)
- [Dicas para evitar erros comuns ao lidar com datas em PHP](https://www.toptal.com/php/solving-the-php-datetime-frustration)