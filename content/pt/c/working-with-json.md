---
title:                "Trabalhando com json"
html_title:           "C: Trabalhando com json"
simple_title:         "Trabalhando com json"
programming_language: "C"
category:             "C"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c/working-with-json.md"
---

{{< edit_this_page >}}

## Por que trabalhar com JSON?

Se você está se aventurando no mundo da programação, é provável que já tenha ouvido falar em JSON. Esta é uma forma popular de armazenar e trocar dados entre diferentes aplicações. Com a crescente demanda pela integração de sistemas e a criação de APIs, é importante entender como trabalhar com JSON em uma linguagem de programação como C.

## Como fazer

Para começar, é preciso ter em mente que JSON é uma estrutura de dados bastante simples. Ele consiste em pares de chave e valor, semelhante a um dicionário. A principal diferença é que os valores podem ser de vários tipos, como strings, números e até mesmo outras estruturas JSON.

Para criar um objeto JSON em C, primeiro precisamos incluir a biblioteca "json-c" em nosso código:

```
#include <stdio.h>
#include <json-c/json.h>
```

Em seguida, podemos criar um objeto JSON básico com a função `json_object` e adicionar pares de chave e valor usando `json_object_object_add`. Vamos dar uma olhada em um exemplo que cria um objeto JSON com duas chaves: "nome" e "idade":

```
struct json_object *obj = json_object_new_object();
json_object_object_add(obj, "nome", json_object_new_string("João"));
json_object_object_add(obj, "idade", json_object_new_int(25));

printf("%s", json_object_to_json_string(obj));
```

A saída deste código seria:

```
{"nome":"João","idade":25}
```

Além de criar objetos JSON, também é possível ler e manipular dados de um arquivo JSON. Supondo que temos um arquivo "dados.json" que contém as informações do exemplo acima, podemos ler e imprimir os valores da seguinte maneira:

```
/* Lê o arquivo */
FILE *arquivo = fopen("dados.json", "r");
struct json_object *obj = json_object_from_fd(fileno(arquivo));

/* Obtém os valores das chaves */
struct json_object *nome;
struct json_object *idade;
json_bool sucesso = json_object_object_get_ex(obj, "nome", &nome);
sucesso = json_object_object_get_ex(obj, "idade", &idade);

/* Imprime os valores */
printf("Nome: %s\n", json_object_to_json_string(nome));
printf("Idade: %d\n", json_object_get_int(idade));
```

A saída seria:

```
Nome: "João"
Idade: 25
```

## Aprofundando-se

Para quem deseja se aprofundar mais no assunto, é importante conhecer as diferentes funções e métodos disponíveis na biblioteca "json-c". Além das já mencionadas `json_object_new_object` e `json_object_object_add`, também há outras como `json_object_get_type`, que retorna o tipo de dado de um objeto JSON, e `json_object_to_json_string_ext`, que permite definir o formato de saída do objeto.

Também é interessante explorar como percorrer e manipular objetos JSON com loops e condicionais. Além disso, é importante se familiarizar com o formato do arquivo JSON e suas possibilidades, como arrays e objetos aninhados.

## Veja também

- [Documentação oficial do json-c](https://json-c.github.io/json-c/)
- [Tutorial em vídeo sobre json-c em C](https://www.youtube.com/watch?v=cGVfIauw25s)
- [Curso completo de introdução ao JSON em C](https://www.udemy.com/course/introducao-json-c/?referralCode=CAFABF66016A387FAAB3)