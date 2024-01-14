---
title:                "PHP: Comparando duas datas"
programming_language: "PHP"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/php/comparing-two-dates.md"
---

{{< edit_this_page >}}

# Por que comparar duas datas em programação pode ser útil?

Comparar datas é uma tarefa comum em muitos projetos de programação, especialmente quando se lida com dados que envolvem eventos ou transações temporais. Ao comparar duas datas, é possível verificar a ordem cronológica entre elas e realizar outras operações, como calcular a diferença entre elas. Neste artigo, iremos explorar como realizar essa tarefa em PHP e algumas dicas para aprofundar ainda mais o conhecimento sobre o assunto.

## Como realizar a comparação em PHP

Em PHP, existem várias funções nativas para comparar datas, como `strtotime()` e `DateTime::diff()`. Vamos ver alguns exemplos de como essas funções podem ser usadas para comparar duas datas em diferentes formatos:

```PHP
<?php 
$data1 = "01/01/2020";
$data2 = "2020-01-01";

// Converter as datas para o formato "timestamp"
$timestamp1 = strtotime($data1);
$timestamp2 = strtotime($data2);

// Utilizar a função nativa "time()" para obter a data atual
$timestampAtual = time();

// Comparar as datas utilizando os operadores de comparação
if($timestamp1 < $timestampAtual){
    echo "A data 1 é anterior à data atual";
} else if($timestamp2 > $timestampAtual){
    echo "A data 2 é posterior à data atual";
} else {
    echo "A data 1 e a data 2 são iguais";
}

// Calcular a diferença entre as datas utilizando a função "DateTime::diff()"
$dataInicial = new DateTime("2020-01-01");
$dataFinal = new DateTime("2020-01-05");
$diferenca = $dataInicial->diff($dataFinal);

echo "A diferença entre as datas é de " . $diferenca->days . " dias.";
?>
```

O código acima irá mostrar a mensagem "A data 1 é anterior à data atual" e "A diferença entre as datas é de 4 dias" como output. É importante notar que antes de comparar as datas, é necessário convertê-las para o formato "timestamp", pois isso facilita a comparação e cálculo de diferença entre elas.

## Aprofundando no assunto

Além das funções nativas mencionadas, existem outras formas de comparar datas em PHP, como utilizando bibliotecas ou criando suas próprias funções. Também é importante ter em mente que ao comparar duas datas, deve-se levar em consideração se ambas estão no mesmo fuso horário. Caso contrário, a comparação pode não ser precisa.

Outro aspecto a se considerar ao trabalhar com datas é a presença de horário de verão ou mudanças de fuso horário em diferentes países. Portanto, é recomendado que ao lidar com datas em um ambiente internacional, sejam utilizados os formatos de data e hora padronizados pela norma ISO 8601.

## Veja também

Para saber mais sobre como comparar datas em PHP, confira os links abaixo:

- [Documentação oficial do PHP sobre funções de data e hora](https://www.php.net/manual/pt_BR/ref.datetime.php)
- [Artigo "Comparando datas em PHP" do blog Curso em Vídeo](https://cursoemvideo.com/blog/comparando-datas-em-php/)
- [Artigo "Funções para trabalhar com datas em PHP" do site Alura](https://www.alura.com.br/artigos/funcoes-e-manipulacao-de-datas-em-php)

Esperamos que este artigo tenha sido útil para entender a importância e como realizar a comparação de datas em PHP. Até a próxima!