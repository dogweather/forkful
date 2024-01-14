---
title:                "PHP: Trabalhando com json"
simple_title:         "Trabalhando com json"
programming_language: "PHP"
category:             "PHP"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/php/working-with-json.md"
---

{{< edit_this_page >}}

## Por que trabalhar com JSON?

JSON (JavaScript Object Notation) é um formato leve e fácil de ler e escrever para a troca de dados entre diferentes sistemas. Se você está trabalhando com transferência de dados via API, integrando sistemas ou criando aplicações web, entender como trabalhar com JSON é essencial.

## Como fazer:

Para trabalhar com JSON em PHP, precisamos entender algumas funções básicas. Primeiramente, podemos usar a função `json_encode()` para converter um objeto ou array em uma string JSON. Veja um exemplo simples:

```PHP
<?php
// Definindo um array com informações de um produto
$produto = [
    "nome" => "Camiseta",
    "preco" => 50.00,
    "tamanho" => "M",
    "cores" => ["branco", "preto", "azul"]
];

// Convertendo o array em uma string JSON
$json = json_encode($produto);

// Imprimindo o resultado
echo $json;
?>
```

Este código irá produzir a seguinte saída:

```JSON
{"nome":"Camiseta","preco":50,"tamanho":"M","cores":["branco","preto","azul"]}
```

Podemos também utilizar a função `json_decode()` para converter uma string JSON em um objeto ou array. Veja um exemplo:

```PHP
<?php
// Definindo uma string JSON
$json = '{"nome":"Camiseta","preco":50,"tamanho":"M","cores":["branco","preto","azul"]}';

// Convertendo a string em um array
$produto = json_decode($json, true);

// Imprimindo o resultado
echo "Nome do produto: " . $produto['nome'] . "<br>";
echo "Preço: R$ " . $produto['preco'] . "<br>";
echo "Tamanho: " . $produto['tamanho'] . "<br>";
echo "Cores disponíveis: " . implode(", ", $produto['cores']);
?>
```

A saída deste código será:

```
Nome do produto: Camiseta
Preço: R$ 50
Tamanho: M
Cores disponíveis: branco, preto, azul
```

## Mergulho profundo:

Além das funções básicas, existem outras maneiras de trabalhar com JSON em PHP, como a utilização de bibliotecas externas, como o *Composer*. Além disso, é importante entender como validar e manipular os dados em formato JSON para evitar erros e problemas de segurança em suas aplicações.

Para uma compreensão mais aprofundada sobre como trabalhar com JSON em PHP, recomenda-se a leitura da documentação oficial da linguagem e a prática em projetos reais.

## Veja também:

- [Documentação oficial do PHP sobre JSON](https://www.php.net/manual/pt_BR/book.json.php)
- [Biblioteca Composer para trabalhar com JSON em PHP](https://github.com/justinrainbow/json-schema)
- [Validação e manipulação de dados em JSON](https://www.w3schools.com/js/js_json_validate.asp)