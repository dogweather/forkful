---
title:                "Trabalhando com CSV"
html_title:           "Bash: Trabalhando com CSV"
simple_title:         "Trabalhando com CSV"
programming_language: "PHP"
category:             "PHP"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/php/working-with-csv.md"
---

{{< edit_this_page >}}

## O Que & Porquê?
Trabalhar com CSV (Comma-Separated Values ou Valores Separados por Vírgula) permite manipular dados textuais de forma estruturada. Programadores fazem isso para importar, exportar e manipular dados entre programas e sistemas de forma simples e rápida.

## Como fazer:
Para manipular arquivos CSV em PHP, vamos usar as funções `fgetcsv` e `fputcsv`. Seguem exemplos de leitura e escrita de um CSV:

Leitura de um arquivo CSV:
```php
<?php
$csvFile = fopen("exemplo.csv", "r");
while (($data = fgetcsv($csvFile, 1000, ",")) !== FALSE) {
    echo "Nome: {$data[0]}, Idade: {$data[1]}\n";
}
fclose($csvFile);
?>
```

Escrita em um arquivo CSV:
```php
<?php
$list = array(
    array('João', '30'),
    array('Ana', '22'),
    array('Carlos', '47')
);

$csvFile = fopen('exemplo_escrita.csv', 'w');
foreach ($list as $line) {
    fputcsv($csvFile, $line);
}
fclose($csvFile);
?>
```

Saída (após ler o `exemplo.csv`):
```
Nome: João, Idade: 30
Nome: Ana, Idade: 22
Nome: Carlos, Idade: 47
```

## Aprofundando
CSV é um formato que existe desde os primeiros dias da computação pessoal. Décadas depois, ainda é relevante devido à sua simplicidade. Embora existam alternativas - como JSON e XML - CSV é amplamente suportado e fácil de compreender. A implementação no PHP é direta, através de funções nativas, que lidam bem com diversas especificidades de diferentes padrões de CSV (delimitadores, qualificadores de texto, etc).

## Veja Também
Para ir além, confira estes recursos:
- Documentação oficial do PHP sobre funções CSV: [php.net/manual/en/ref.funchand.php](https://www.php.net/manual/en/ref.funchand.php)
- Guia de boas práticas com CSV: [The CSV on the Web Working Group (CSVW)](https://www.w3.org/2013/csvw/)
- Para mais sobre manipulação de arquivos em PHP: [php.net/manual/en/book.filesystem.php](https://www.php.net/manual/en/book.filesystem.php)
