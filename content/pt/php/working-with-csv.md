---
title:                "Trabalhando com csv"
html_title:           "PHP: Trabalhando com csv"
simple_title:         "Trabalhando com csv"
programming_language: "PHP"
category:             "PHP"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/php/working-with-csv.md"
---

{{< edit_this_page >}}

## Por que trabalhar com CSV?

O formato CSV (Comma Separated Values) é amplamente utilizado para armazenar e compartilhar dados tabulares, como planilhas e bancos de dados. Trabalhar com CSV em PHP permite que você leia e escreva facilmente esses dados em seu código, facilitando a manipulação e análise de grandes quantidades de informações.

## Como fazer

Para começar a trabalhar com CSV em PHP, você precisará:

- Ter o PHP instalado em seu computador ou servidor
- Conhecer os conceitos básicos de leitura e escrita de arquivos em PHP

Abaixo estão alguns exemplos de código que mostram como ler e escrever dados em um arquivo CSV usando PHP.

#### Lendo dados de um arquivo CSV

```
<?php
$handle = fopen('arquivo.csv', 'r');
if ($handle) {
    while (($line = fgetcsv($handle)) !== false) {
        // processar os dados do CSV aqui
        print_r($line);
    }
    fclose($handle);
}
?>
```

Este código irá abrir o arquivo contendo os dados CSV e ler linha por linha, armazenando as informações em um array. Você pode então manipular esses dados da maneira que desejar.

#### Escrevendo dados em um arquivo CSV

```
<?php
$dados = [
    ['nome', 'email', 'telefone'],
    ['João', 'joao@email.com', '111111111'],
    ['Maria', 'maria@email.com', '222222222']
];

$f = fopen('arquivo.csv', 'w');
foreach ($dados as $linha) {
    fputcsv($f, $linha);
}
fclose($f);
```

Este código criará um novo arquivo CSV e escreverá os dados armazenados no array na forma de linhas com valores separados por vírgulas.

## Profundidade

A função `fgetcsv()` usada no primeiro exemplo aceita vários parâmetros opcionais, como o delimitador de valores e o caractere de escape. Isso permite uma maior personalização da leitura de dados CSV, dependendo do formato do arquivo que você está trabalhando.

Além disso, o PHP possui a classe [SplFileObject](https://www.php.net/manual/pt_BR/class.splfileobject.php) que permite a leitura e manipulação de dados CSV de maneira mais eficiente e flexível.

Veja também:

- [Documentação do PHP sobre manipulação de arquivos CSV](https://www.php.net/manual/pt_BR/function.fgetcsv.php)
- [Tutorial sobre leitura e escrita de CSV em PHP](https://www.tutorialspoint.com/php/php_and_csv.htm)
- [Tutorial de PHP e CSV na prática](https://www.sitepoint.com/using-csv-files-php/)
- [Mais sobre a classe SplFileObject](https://www.php.net/manual/pt_BR/class.splfileobject.php)

## Veja também

- [Leia e escreva dados em CSV em Python](https://www.datacamp.com/community/tutorials/working-with-csv-files-in-python)
- [Manipulação de dados CSV em JavaScript](https://www.sitepoint.com/reading-and-writing-csv-files-with-javascript/)
- [Como importar e exportar dados CSV em MySQL](https://www.mysqltutorial.org/import-csv-file-mysql-table/)