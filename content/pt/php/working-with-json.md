---
title:                "Trabalhando com json"
html_title:           "PHP: Trabalhando com json"
simple_title:         "Trabalhando com json"
programming_language: "PHP"
category:             "PHP"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/php/working-with-json.md"
---

{{< edit_this_page >}}

## O que & Porquê?
JSON (JavaScript Object Notation) é um formato de arquivo leve e de fácil leitura que é usado para troca de dados entre aplicativos e servidores web. Os programadores o usam para estruturar dados em um formato semelhante à linguagem JavaScript, tornando fácil o transporte e a interpretação desses dados em diferentes plataformas e linguagens de programação.

## Como fazer:
Vamos dar uma olhada em como usar JSON em seu código PHP!

### Decodificar dados JSON
Use a função `json_decode()` para transformar uma string JSON em um objeto PHP utilizável. Por exemplo:
```PHP
$json = '{"nome": "Maria", "idade": 25}';
$dados = json_decode($json);

echo $dados->nome; //saída: Maria
echo $dados->idade; //saída: 25
```

### Codificar dados em JSON
Para criar uma string JSON a partir de um objeto ou array em PHP, use a função `json_encode()`. Por exemplo:
```PHP
$usuario = [
  'nome' => 'João',
  'idade' => 30,
  'hobbies' => ['futebol', 'cozinhar', 'viajar']
];

$json = json_encode($usuario);

echo $json; //saída: {"nome": "João", "idade": 30, "hobbies": ["futebol", "cozinhar", "viajar"]}
```

## Mergulho Profundo:
JSON foi criado por Douglas Crockford em 2001 e é inspirado na sintaxe de objetos JavaScript. É amplamente utilizado em comunicações de rede e APIs RESTful devido à sua simplicidade e compatibilidade com várias linguagens de programação.

Alternativas para JSON incluem XML, YAML e o formato serializado de PHP. No entanto, JSON tem se destacado como um formato popular para transferência de dados.

Quando se trabalha com JSON, é importante ter em mente que ele suporta apenas um subconjunto de tipos de dados, como strings, números, objetos, arrays e valores booleanos. Ele também não tem suporte para comentários, então é recomendado que seja usado apenas para transferência de dados e não como um formato de configuração.

## Veja também:
- [Documentação Oficial do PHP para Funções JSON](https://www.php.net/manual/en/ref.json.php)
- [Tutorial de Introdução ao JSON da W3Schools](https://www.w3schools.com/js/js_json_intro.asp)
- [Introdução ao JSON na MDN Web Docs](https://developer.mozilla.org/en-US/docs/Learn/JavaScript/Objects/JSON)