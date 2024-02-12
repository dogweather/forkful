---
title:                "Trabalhando com JSON"
aliases: - /pt/php/working-with-json.md
date:                  2024-02-03T19:23:43.358131-07:00
model:                 gpt-4-0125-preview
simple_title:         "Trabalhando com JSON"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/php/working-with-json.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## O Que & Por Quê?
JSON, ou JavaScript Object Notation, é um formato leve de troca de dados fácil de ler e escrever por humanos, e fácil de analisar e gerar por máquinas. Programadores frequentemente trabalham com JSON para trocar dados entre servidores e aplicações web devido à sua simplicidade e independência de linguagem, tornando-o um pilar no desenvolvimento web moderno e nas APIs.

## Como Fazer:
Trabalhar com JSON em PHP é direto graças às funções integradas `json_encode()` e `json_decode()`. Abaixo estão exemplos mostrando como converter um array PHP em uma string JSON e vice-versa:

### Codificando um Array PHP em uma String JSON
```php
// Definir um array associativo
$data = [
    "name" => "John Doe",
    "age" => 30,
    "email" => "john.doe@example.com"
];

// Converter o array PHP para uma string JSON
$jsonString = json_encode($data);

// Exibir a string JSON
echo $jsonString;
```
**Saída de Exemplo:**
```json
{"name":"John Doe","age":30,"email":"john.doe@example.com"}
```

### Decodificando uma String JSON em um Array PHP
```php
// String JSON
$jsonString = '{"name":"John Doe","age":30,"email":"john.doe@example.com"}';

// Converter a string JSON em um array PHP
$data = json_decode($jsonString, true);

// Exibir o array PHP
print_r($data);
```
**Saída de Exemplo:**
```
Array
(
    [name] => John Doe
    [age] => 30
    [email] => john.doe@example.com
)
```

### Trabalhando com uma Biblioteca de Terceiros: GuzzleHttp
Para manipulação complexa de JSON e solicitações web, uma biblioteca PHP popular é o GuzzleHttp. Ela simplifica solicitações HTTP e trabalha facilmente com dados JSON.

**Instalação via Composer:**
```
composer require guzzlehttp/guzzle
```

**Exemplo de Solicitação:**
```php
require 'vendor/autoload.php';

use GuzzleHttp\Client;

$client = new Client();

// Enviar uma solicitação para uma API que retorna JSON
$response = $client->request('GET', 'https://api.example.com/data', [
    'headers' => [
        'Accept' => 'application/json',
    ],
]);

// Decodificar a resposta JSON para um array PHP
$data = json_decode($response->getBody(), true);

// Exibir os dados
print_r($data);
```

**Assumindo que a API retorna dados JSON similares:**
```
Array
(
    [name] => John Doe
    [age] => 30
    [email] => john.doe@example.com
)
```
Isso demonstra a facilidade de usar PHP para manipulação de JSON, tanto com funções nativas quanto com bibliotecas robustas como o GuzzleHttp para tarefas mais complexas.
