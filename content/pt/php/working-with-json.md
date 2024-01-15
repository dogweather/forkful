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

## Por que trabalhar com JSON?

JSON (JavaScript Object Notation) é um formato de dados leve e popular que é amplamente usado na comunicação de sistemas na web. Ao se familiarizar com JSON e aprender a trabalhar com ele em PHP, você poderá criar aplicativos da web mais dinâmicos e eficientes em termos de processamento de dados.

## Como fazer

Para começar a trabalhar com JSON em PHP, você pode seguir os seguintes passos:

1. Instale o PHP em seu sistema, se ainda não o tiver.
2. Crie um arquivo PHP e salve-o com a extensão ".php".
3. Inicie um servidor local para testar suas instruções PHP.
4. Importe o arquivo JSON que deseja trabalhar em seu código PHP usando a função `file_get_contents()`.
5. Use a função `json_decode()` para decodificar o conteúdo do arquivo JSON em um array ou objeto, que você pode manipular em seu código PHP.
6. Se precisar converter um array ou objeto PHP em JSON, use a função `json_encode()`.
7. Para interagir com outros serviços ou aplicativos que usam JSON, você precisará aprender sobre APIs e como fazer solicitações HTTP em PHP.

Aqui está um exemplo de como importar e manipular um arquivo JSON usando PHP:

```PHP
<?php
// Importando o arquivo JSON
$json_data = file_get_contents('dados.json');

// Decodificando o conteúdo em um objeto
$obj = json_decode($json_data);

// Acessando os dados do objeto
echo 'Nome: ' . $obj->nome . '<br>'; // Imprime "Nome: João"
echo 'Idade: ' . $obj->idade . '<br>'; // Imprime "Idade: 25"
echo 'Hobbies: ' . implode(', ', $obj->hobbies) . '<br>'; // Imprime "Hobbies: ler, viajar, cozinhar"
?>
```

## Aprofundando-se

Aqui estão alguns tópicos que você pode explorar para se familiarizar mais com o trabalho com JSON em PHP:

- Aprendendo sobre as funções de manipulação de JSON disponíveis em PHP, como `json_last_error()` e `json_last_error_msg()` para lidar com erros ao decodificar ou codificar JSON.
- Descobrindo como usar a opção `assoc` na função `json_decode()` para decodificar o JSON em uma matriz associativa em vez de um objeto.
- Conhecendo as funções `json_decode_object()` e `json_encode_object()` para decodificar ou codificar JSON em objetos JSON nativos.
- Utilizando a opção `pretty_print` na função `json_encode()` para tornar o JSON mais legível na saída.
- Aprendendo como utilizar as classes `JsonSerializable` e `Serializable` para tornar seus objetos PHP convertíveis em JSON.
- Descobrindo como manipular dados JSON aninhados e como acessar diferentes níveis de profundidade em uma estrutura JSON.
- Familiarizando-se com as melhores práticas para trabalhar com JSON em PHP, incluindo como validar e sanitizar dados JSON recebidos de fontes externas.

## Veja também

- Documentação oficial do PHP sobre JSON: https://www.php.net/manual/pt_BR/book.json.php
- Um tutorial abrangente sobre como trabalhar com JSON em PHP: https://www.digitalocean.com/community/tutorials/how-to-use-json-data-from-a-file-in-php
- Um guia sobre como usar APIs em PHP, incluindo como fazer solicitações HTTP e trabalhar com dados JSON: https://www.freecodecamp.org/news/how-to-use-an-api-with-php-4ce531e6acc9/