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

O que & Porquê?
CSV (Comma Separated Values) é uma forma de armazenar e gerenciar dados tabulares em um documento de texto simples, onde os valores são separados por vírgulas. Programadores utilizam CSV para importar e exportar dados de forma eficiente, especialmente em aplicações web onde é necessário transferir grandes quantidades de informações entre sistemas.

Como fazer:
Para trabalhar com CSV em PHP, utilize as funções built-in ```fopen()``` e ```fgetcsv()``` para abrir e ler o arquivo CSV. O exemplo a seguir busca por uma linha específica no arquivo e exibe seu conteúdo.

```
<?php

// abre o arquivo CSV
$file = fopen('dados.csv', 'r');

// percorre as linhas do arquivo
while (($line = fgetcsv($file)) !== FALSE) {

  // verifica se a linha desejada foi encontrada
  if ($line[0] === '1234') {

    // exibe os dados da linha
    echo "ID: " . $line[0] . "\n";
    echo "Nome: " . $line[1] . "\n";
    echo "Sobrenome: " . $line[2] . "\n";
    break;
  }
}

// fecha o arquivo
fclose($file);

// saída:
// ID: 1234
// Nome: Maria
// Sobrenome: Silva
```

## Deep Dive
CSV surgiu nos anos 70 como um formato simples para transferir dados entre computadores. Em anos mais recentes, o formato ganhou popularidade devido à sua flexibilidade e suporte em diferentes softwares e linguagens. Uma alternativa ao CSV é o formato JSON, mais utilizado em aplicações web.

O processo de escrita em um arquivo CSV é semelhante à leitura, utilizando a função ```fputcsv()```. É importante tomar cuidado com caracteres especiais e definir o delimitador e o separador de campos corretamente.

## See Also
- [Documentação oficial do PHP para funções de manipulação de arquivos CSV](https://www.php.net/manual/en/ref.filesystem.php)
- [Mais detalhes sobre o formato CSV](https://en.wikipedia.org/wiki/Comma-separated_values)