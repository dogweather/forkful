---
title:    "PHP: Comparando duas datas"
keywords: ["PHP"]
---

{{< edit_this_page >}}

Por que comparar duas datas em PHP?

Comparar datas é uma tarefa comum em programação, especialmente em projetos que envolvem controle de datas, agendamentos ou cálculos temporais. Ao comparar duas datas em PHP, podemos determinar a diferença entre elas, verificar se uma data está no passado ou futuro, ou até mesmo calcular o intervalo de tempo entre elas.

Como fazer:

Em PHP, podemos comparar duas datas utilizando a função "strtotime". Essa função converte uma data em formato de texto em um inteiro que representa o número de segundos desde 01 de janeiro de 1970. Vamos ver um exemplo:

```PHP
// Definindo duas datas
$data1 = "2019-10-01";
$data2 = "2020-10-01";

// Convertendo as datas em inteiros
$data1_int = strtotime($data1);
$data2_int = strtotime($data2);

// Comparando as duas datas
if ($data1_int < $data2_int) {
    echo "A data 1 é anterior à data 2.";
} else if ($data1_int > $data2_int) {
    echo "A data 1 é posterior à data 2.";
} else {
    echo "As datas são iguais.";
}

// Output: A data 1 é anterior à data 2.
```

Podemos também utilizar a função "date_diff" para obter a diferença entre duas datas em anos, meses, dias, horas, minutos e segundos. Vamos dar uma olhada:

```PHP
// Definindo duas datas
$data1 = "2019-05-15";
$data2 = "2020-05-20";

// Convertendo as datas em objetos DateTime
$data1_dt = new DateTime($data1);
$data2_dt = new DateTime($data2);

// Obtendo a diferença entre as datas
$diferenca = date_diff($data1_dt, $data2_dt);

// Imprimindo o resultado
echo $diferenca->format('%y anos, %m meses, %d dias');

// Output: 1 ano, 0 meses, 5 dias
```

Deep Dive:

Ao comparar duas datas em PHP, é importante ter em mente o formato das datas utilizadas. Por padrão, o formato aceito é "Y-m-d" (Ano-Mês-Dia), mas também é possível utilizar outros formatos, como "d/m/Y" (Dia/Mês/Ano). Além disso, devemos prestar atenção nos tipos de dados utilizados, pois a função "strtotime" só aceita datas em formato de texto.

Ao utilizar a função "strtotime", também é possível passar um parâmetro adicional para informar a data de referência. Por exemplo, se quisermos verificar se uma data está antes ou depois de outra, podemos utilizar uma data de referência como parâmetro, assim a comparação será feita em relação a essa data. Isso pode ser útil em casos como agendamentos ou cálculos de juros.

Outra função útil para comparação de datas é a "checkdate", que verifica se uma data é válida ou não. Podemos combinar essa função com a função "strtotime" para garantir que as datas utilizadas no código sejam válidas.

Veja também:

- Documentação oficial do PHP para a função "strtotime": https://www.php.net/manual/en/function.strtotime.php
- Documentação oficial do PHP para a função "date_diff": https://www.php.net/manual/en/function.date-diff.php
- Documentação oficial do PHP para a função "checkdate": https://www.php.net/manual/en/function.checkdate.php