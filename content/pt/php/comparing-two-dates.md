---
title:    "PHP: Comparando duas datas"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/php/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Por que Comparar Duas Datas é Importante para a Programação em PHP 

Comparar datas é uma tarefa comum na programação em PHP. Isso pode ser necessário para verificar a validade de uma entrada do usuário, para realizar cálculos de tempo ou para ordenar dados por data. Compreender como comparar duas datas corretamente é essencial para garantir que sua aplicação funcione corretamente e forneça os resultados esperados. Neste artigo, vamos explorar os conceitos básicos de comparação de datas em PHP e fornecer exemplos práticos.

## Como Comparar Duas Datas Usando PHP 

Em PHP, existem várias funções que podem ser usadas para comparar datas. A função `strtotime()` pode ser usada para converter uma data em formato de texto para um timestamp, que é um valor numérico que representa a data em segundos. Isso facilita a comparação de datas, pois você pode simplesmente comparar os valores numéricos e obter um resultado preciso. Aqui está um exemplo de como comparar duas datas usando a função `strtotime()`:

```PHP
// Definindo duas datas para comparação
$data1 = "2021-01-01";
$data2 = "2021-01-15";

// Convertendo as datas em timestamps
$timestamp1 = strtotime($data1);
$timestamp2 = strtotime($data2);

// Comparando os timestamps
if ($timestamp1 < $timestamp2) {
    echo "A data 1 ocorre antes da data 2";
} elseif ($timestamp1 > $timestamp2) {
    echo "A data 1 ocorre depois da data 2";
} else {
    echo "As duas datas são iguais";
}
```

Neste exemplo, as datas são convertidas em timestamps e depois comparadas usando uma estrutura condicional. Se a data 1 for anterior à data 2, a mensagem "A data 1 ocorre antes da data 2" será exibida. Se for o contrário, a mensagem "A data 1 ocorre depois da data 2" será exibida. Se as duas datas forem iguais, a mensagem "As duas datas são iguais" será exibida.

Além disso, existem outras funções úteis para comparação de datas, como `date_diff()` e `DateTime::diff()`, que podem ser usadas para calcular a diferença entre duas datas em dias, horas, minutos, etc. Você pode consultar a documentação do PHP para obter mais informações sobre essas funções e como usá-las.

## Mergulho Profundo na Comparação de Datas em PHP 

Ao comparar datas em PHP, é importante levar em consideração alguns fatores para garantir resultados precisos. Aqui estão algumas coisas a se ter em mente ao trabalhar com comparação de datas:

- Certifique-se de que o formato das datas esteja correto. O formato padrão no PHP é `YYYY-MM-DD`, mas você pode usar a função `date()` para formatar as datas de acordo com sua preferência.
- Tenha em mente que os meses em PHP são representados por números, sendo janeiro o mês 1 e dezembro o mês 12. Ao usar datas em diferentes formatos, pode ser necessário fazer alguns ajustes antes de compará-las.
- Considere o fuso horário ao trabalhar com datas e timestamps. Se sua aplicação estiver em um servidor com fuso horário diferente do seu, os resultados da comparação podem ser diferentes do esperado.

Lembre-se de que existem muitas maneiras diferentes de comparar datas em PHP e nem todas as soluções se aplicam a todas as situações. É importante entender os conceitos básicos e adaptá-los de acordo com suas necessidades específicas.

## Veja Também 

- [Documentação do PHP sobre funções de comparação de datas](https://www.php.net/manual/en/datetime.diff.php)
- [Tutorial de comparação de datas em PHP da W3Schools (em português)](https://www.w3schools.com/php/php_date.asp)
- [Exemplos de uso de funções de comparação de datas em PHP](https://www.php.net/manual/en/datetime.settimezone.php)

Esperamos que este artigo tenha sido útil para você entender melhor como comparar duas datas em PHP. Com esses conhecimentos, você poderá facilmente implementar essa funcionalidade em suas aplicações e garantir resultados precisos. Não deixe de explorar outras funções e recursos