---
title:                "Trabalhando com JSON"
html_title:           "Arduino: Trabalhando com JSON"
simple_title:         "Trabalhando com JSON"
programming_language: "PHP"
category:             "PHP"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/php/working-with-json.md"
---

{{< edit_this_page >}}

## O Que é & Porquê?

Trabalhar com JSON significa lidar com um formato leve de troca de dados, fácil de ler e escrever para humanos e máquinas. Programadores usam isso pela sua simplicidade e interoperabilidade entre diferentes linguagens e plataformas.

## Como Fazer:

```PHP
<?php
// Criando um array em PHP.
$data = array("nome" => "João", "idade" => 25, "cidade" => "Lisboa");

// Convertendo o array em JSON.
$jsonData = json_encode($data);

echo $jsonData;
// Saída: {"nome":"João","idade":25,"cidade":"Lisboa"}

// Convertendo JSON de volta para PHP array.
$json = '{"nome":"Ana","idade":22,"cidade":"Porto"}';
$dataArray = json_decode($json, true);

print_r($dataArray);
// Saída: Array ( [nome] => Ana [idade] => 22 [cidade] => Porto )
?>
```

## Aprofundando:

O JSON (JavaScript Object Notation) é um formato criado por Douglas Crockford nos anos 2000. Alternativas a JSON incluem XML e YAML, mas JSON prevalece pelas suas vantagens em termos de eficiência e acessibilidade. Na implementação em PHP, funções como `json_encode()` e `json_decode()` são essenciais para converter arrays e objectos PHP para JSON e vice-versa, respetivamente.

## Veja Também:

- Documentação oficial do PHP para JSON: [php.net/manual/pt_BR/book.json.php](https://www.php.net/manual/pt_BR/book.json.php)
- Tutorial de JSON: [w3schools.com/js/js_json_intro.asp](https://www.w3schools.com/js/js_json_intro.asp)
- História do JSON por Douglas Crockford: [json.org](https://www.json.org)