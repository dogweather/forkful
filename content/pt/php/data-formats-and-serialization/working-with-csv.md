---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:20:38.837225-07:00
description: "Trabalhar com CSV (Comma-Separated Values ou Valores Separados por V\xED\
  rgula) envolve ler e escrever dados em arquivos CSV, um formato popular para\u2026"
lastmod: '2024-03-13T22:44:46.689600-06:00'
model: gpt-4-0125-preview
summary: "Trabalhar com CSV (Comma-Separated Values ou Valores Separados por V\xED\
  rgula) envolve ler e escrever dados em arquivos CSV, um formato popular para\u2026"
title: Trabalhando com CSV
weight: 37
---

## O Que & Por Que?

Trabalhar com CSV (Comma-Separated Values ou Valores Separados por Vírgula) envolve ler e escrever dados em arquivos CSV, um formato popular para representar dados tabulares em texto plano. Programadores fazem isso para trocar dados facilmente entre diferentes programas, sistemas ou bancos de dados, graças à sua simplicidade e amplo suporte em plataformas e linguagens de programação.

## Como fazer:

O PHP oferece funções integradas para manipulação de arquivos CSV, tornando direto ler e escrever nesses arquivos sem necessidade de bibliotecas de terceiros. Aqui estão exemplos para começar:

### Lendo um Arquivo CSV

Você pode abrir um arquivo CSV e ler seu conteúdo usando `fopen()` em combinação com `fgetcsv()`:

```php
<?php
$nomeArquivo = 'data.csv';
$manipulador = fopen($nomeArquivo, "r");
if ($manipulador !== FALSE) {
    while (($dados = fgetcsv($manipulador, 1000, ",")) !== FALSE) {
        $num = count($dados);
        echo "Número de campos na linha: $num\n";
        for ($c = 0; $c < $num; $c++) {
            echo $dados[$c] . "\n";
        }
    }
    fclose($manipulador);
}
?>
```

Este script imprime o número de campos de cada linha seguido pelo conteúdo de cada campo.

### Escrevendo em um Arquivo CSV

Para escrever em um arquivo CSV, use `fopen()` no modo de escrita (`w`) e `fputcsv()`:

```php
<?php
$lista = [
    ['ID', 'Nome', 'Email'],
    [1, 'John Doe', 'john@example.com'],
    [2, 'Jane Doe', 'jane@example.com']
];

$manipulador = fopen('users.csv', 'w');

foreach ($lista como $linha) {
    fputcsv($manipulador, $linha);
}

fclose($manipulador);
?>
```

Este script cria um arquivo chamado `users.csv` e escreve o cabeçalho e duas linhas de dados nele.

### Usando uma Biblioteca: League\Csv

Para manipulação mais avançada de CSV, a biblioteca `League\Csv` oferece um robusto conjunto de funcionalidades. Depois de instalá-la via Composer (`composer require league/csv`), você pode usá-la para ler e escrever dados CSV de maneira mais flexível.

#### Lendo com League\Csv

```php
<?php
require 'vendor/autoload.php';

use League\Csv\Reader;

$csv = Reader::createFromPath('data.csv', 'r');
$csv->setHeaderOffset(0); // Defina se você deseja usar a primeira fila como cabeçalho

$resultados = $csv->getRecords();
foreach ($resultados as $linha) {
    print_r($linha);
}
?>
```

Este script lê `data.csv`, tratando a primeira linha como cabeçalhos de colunas e imprime cada linha como um array associativo.

#### Escrevendo com League\Csv

```php
<?php
require 'vendor/autoload.php';

use League\Csv\Writer;

$csv = Writer::createFromPath('users_new.csv', 'w+');

$csv->insertOne(['ID', 'Nome', 'Email']);
$csv->insertAll([
    [3, 'Alex Doe', 'alex@example.com'],
    [4, 'Anna Smith', 'anna@example.com']
]);

echo "Escrito em users_new.csv com sucesso.";
?>
```

Isso cria `users_new.csv` e escreve uma linha de cabeçalho seguida por duas linhas de dados.
