---
title:    "PHP: Calculando uma data no futuro ou passado"
keywords: ["PHP"]
---

{{< edit_this_page >}}

## Por que

Calcular uma data no futuro ou no passado é uma tarefa comum em programação PHP. Isso permite que os desenvolvedores criem aplicações dinâmicas que exibem informações precisas de acordo com a data atual ou uma data específica. Além disso, é uma habilidade útil para manipular dados em bancos de dados ou realizar operações matemáticas com datas.

## Como fazer

Há várias maneiras de calcular uma data no futuro ou no passado em PHP. Uma delas é usando a função `strtotime` que converte uma string em uma data e permite a manipulação dessa data. Veja um exemplo abaixo:

```PHP
$data_atual = date('d/m/Y'); //captura a data atual
$data_futura = strtotime('+1 week', strtotime($data_atual)); //adiciona 1 semana à data atual
echo date('d/m/Y', $data_futura); //exibe a data futura formatada
```

Neste exemplo, usamos a função `date` para capturar a data atual e a função `strtotime` para adicionar uma semana a essa data. Depois, usamos a função `date` novamente para formatar a data futura e exibi-la no formato desejado. Você também pode adicionar ou subtrair qualquer unidade de tempo, como dias, meses ou anos, usando essa mesma lógica.

## Mergulho profundo

Para entender melhor como calcular datas no futuro ou no passado em PHP, é importante entender como o PHP lida com datas. As datas são representadas em formato Unix, que é o número de segundos desde 1º de janeiro de 1970. Ao usar a função `strtotime`, você está basicamente convertendo uma string em um número Unix que pode ser manipulado e convertido de volta em uma data.

Além disso, o PHP possui uma extensa documentação sobre formatação e manipulação de datas. É sempre recomendável consultar a documentação oficial para aprender mais sobre as diferentes funções e opções disponíveis.

## Veja também

- [Documentação oficial do PHP sobre datas](https://www.php.net/manual/en/function.date.php)
- [Guia completo sobre manipulação de datas em PHP](https://www.w3schools.com/PHP/php_date.asp)
- [Trabalhando com datas e horários em PHP](https://www.php.net/manual/en/datetime.format.php)