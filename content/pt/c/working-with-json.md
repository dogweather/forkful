---
title:                "C: Trabalhando com json"
simple_title:         "Trabalhando com json"
programming_language: "C"
category:             "C"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c/working-with-json.md"
---

{{< edit_this_page >}}

## Por que trabalhar com JSON em programação em C?

JSON (JavaScript Object Notation) é um formato de arquivo amplamente usado em programação, especialmente em aplicações web e de API. Ele permite estruturar e transmitir dados de forma rápida e eficiente, o que o torna uma escolha popular para troca de informações entre sistemas. Se você está interessado em desenvolver aplicações que se conectam à internet ou consumir dados de uma API, é importante conhecer e saber trabalhar com JSON em C.

## Como trabalhar com JSON em C

Antes de iniciar, é importante ter um bom entendimento de como JSON é estruturado. Ele consiste em pares de chave-valor, onde a chave é uma string e o valor pode ser qualquer tipo de dado válido em JSON: string, número, booleano, objeto ou array. Aqui está um exemplo de JSON válido:

```C
{
    "nome": "João",
    "idade": 30,
    "casado": true
}
```
Para começar a trabalhar com JSON em C, é necessário incluir o arquivo de cabeçalho `json-c/json.h`. A biblioteca JSON-C é uma das mais populares para manipulação de JSON em C e pode ser encontrada em diversas plataformas e sistemas operacionais. Uma vez que a biblioteca está incluída, a função `json_object` pode ser utilizada para criar um novo objeto JSON vazio. Em seguida, é possível adicionar pares de chave-valor ao objeto utilizando funções como `json_object_object_add` e `json_object_array_add`. Aqui está um exemplo de como criar e imprimir um objeto JSON utilizando a biblioteca JSON-C:

```C 
// inclui biblioteca JSON-C
#include <json-c/json.h>

int main()
{
    // cria objeto JSON vazio
    json_object *obj = json_object_new_object();

    // adiciona pares de chave-valor ao objeto
    json_object_object_add(obj, "nome", json_object_new_string("João"));
    json_object_object_add(obj, "idade", json_object_new_int(30));
    json_object_object_add(obj, "casado", json_object_new_boolean(true));

    // imprime objeto JSON
    printf("%s\n", json_object_to_json_string(obj));

    return 0;
}

```

A saída deste código será o objeto JSON apresentado anteriormente:
```C
{
    "nome": "João",
    "idade": 30,
    "casado": true
}
```

## Explorando mais sobre JSON em C

Há muitas outras funções disponíveis na biblioteca JSON-C para manipulação e validação de JSON em C. É possível, por exemplo, carregar um arquivo JSON existente através da função `json_object_from_file`, ou converter um objeto JSON em uma string com a função `json_object_to_json_string`. Além disso, existem funções para verificar o tipo de um valor em JSON, acessar elementos específicos em objetos e arrays, entre outras possibilidades.

Além da biblioteca JSON-C, há também outras opções disponíveis para trabalhar com JSON em C, como a jsmn (https://github.com/zserge/jsmn) e a jansson (https://github.com/akheron/jansson). Cada uma delas possui suas próprias características e vantagens, então é importante experimentar e encontrar a opção que melhor se encaixa nas necessidades do seu projeto.

## Veja também
- [Tutorial em vídeo de como trabalhar com JSON em C](https://www.youtube.com/watch?v=JPcry2F1Yxs)
- [Documentação da biblioteca JSON-C oficial](https://github.com/json-c/json-c/)
- [Exemplos práticos de como trabalhar com JSON em C](https://www.learn-c.org/en/JSON)