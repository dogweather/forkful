---
title:                "PHP: Trabalhando com csv"
simple_title:         "Trabalhando com csv"
programming_language: "PHP"
category:             "PHP"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/php/working-with-csv.md"
---

{{< edit_this_page >}}

## Porque trabalhar com CSV?

CSV (Comma Separated Values) é um formato de arquivo amplamente utilizado para armazenar e trocar dados tabulares. Ele é amplamente suportado por muitas linguagens de programação e aplicativos de planilhas, tornando-o uma escolha popular para importar e exportar dados. Ao trabalhar com CSV em PHP, é possível automatizar tarefas que envolvem a manipulação de grandes conjuntos de dados, economizando tempo e esforço.

## Como fazer

Para começar a trabalhar com CSV em PHP, é necessário primeiro abrir o arquivo CSV com a função `fopen()`. Em seguida, pode-se ler os dados usando um loop `while` e a função `fgetcsv()`, que retorna uma linha de dados como um array. Depois de manipular os dados, eles podem ser escritos em um novo arquivo CSV usando as funções `fopen()` e `fputcsv()`. Por exemplo:

```PHP
// Abrir arquivo CSV para leitura
$csv_file = fopen('exemplo.csv', 'r');

// Loop para ler cada linha do arquivo
while (($linha = fgetcsv($csv_file)) !== FALSE) {
    $nome = $linha[0];
    $email = $linha[1];

    // Exibir dados do CSV
    echo "Nome: $nome\n";
    echo "E-mail: $email\n";
}
```

A saída desse código seria algo como:

```
Nome: João Silva
E-mail: joao@example.com
Nome: Maria Santos
E-mail: maria@example.com
```

## Deep Dive

Além das funções mencionadas acima, o PHP também tem outras opções para trabalhar com CSV, como a classe [SplFileObject](https://www.php.net/manual/pt_BR/class.splfileobject.php), que oferece recursos mais avançados de manipulação de arquivos. Além disso, também é possível trabalhar com CSV usando bibliotecas externas, como [League CSV](https://csv.thephpleague.com/).

Outro aspecto importante a ser considerado ao trabalhar com CSV é o formato de arquivo usado. Embora o formato CSV seja amplamente suportado, é importante garantir que o arquivo esteja no formato correto e não tenha erros de formatação, para evitar problemas de leitura e escrita dos dados.

## Veja também

- [Documentação do PHP sobre manipulação de arquivos CSV](https://www.php.net/manual/pt_BR/function.fputcsv.php)
- [Tutorial da DigitalOcean sobre como importar dados CSV em PHP](https://www.digitalocean.com/community/tutorials/how-to-import-and-export-csv-files-using-php-and-mysql)